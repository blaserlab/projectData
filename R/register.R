build_collection <- function(package, lib.loc, id, suffix, include) {
  inst <- resolve_installation(package, lib.loc)
  if (!is_scalar_string(id)) pd_abort("Collection ids must be non-empty strings.")
  if (!is.na(suffix) && !nzchar(suffix)) {
    pd_abort("`suffix` must be a non-empty string (use `primary` to keep bare names).")
  }
  context <- list(collection = id, package = package, version = inst$version,
                  library = inst$library, resource = NA_character_)
  descriptors <- assemble_descriptors(inst$root, context)
  if (!length(descriptors)) {
    pd_abort("Package '", package, "' (", inst$root, ") provides no inventoried data ",
             "resources and no manifest or registered loader describes any.",
             class = "projectData_empty_collection")
  }
  objects <- objects_table(descriptors, include)
  if (!is.null(include)) {
    unknown <- setdiff(include, c(objects$object, objects$resource))
    if (length(unknown)) {
      pd_abort("`include` names not found in '", package, "': ", quote_names(unknown),
               ". Mappings marked 'unknown' cannot be selected until inspected ",
               "(see data_inspect()).")
    }
  }
  resources <- stats::setNames(lapply(descriptors, function(d) new_resource(id, d)),
                               vapply(descriptors, function(d) d$resource, ""))
  list(id = id, package = package, version = inst$version, identity = inst$identity,
       library = inst$library, root = inst$root, suffix = suffix, include = include,
       resources = resources, objects = objects,
       env = new.env(parent = emptyenv()))
}

#' Register installed data collections
#'
#' Builds an inventory of each package's data and records it in the session.
#' **No data values are read**: inventory uses the package's installed data
#' index, the lazy-load name map, an optional manifest, and discovery by
#' loaders you have registered. Registration never attaches anything, and it
#' is atomic: if any collection fails validation, nothing changes.
#'
#' The installed copy is resolved once, here, from `lib.loc` (default
#' [.libPaths()]), and the root, library, version and build identity are
#' recorded. Later changes to [.libPaths()] cannot redirect a pending read.
#' If the installed package is replaced in place before a resource is opened,
#' opening fails with an informative error and the collection must be
#' re-registered with `replace = TRUE`; objects already opened are unaffected.
#'
#' @section Selective registration:
#' `include` limits which objects are exposed (by object or resource name).
#' Opening one object loads its whole resource, so sibling objects from the
#' same resource are opened (and cached) together even if not exposed.
#'
#' @section Mapping completeness:
#' The catalog's `mapping` column is `index` (from install-time metadata),
#' `declared` (manifest), `unknown` (resource exists, objects not known), or
#' `observed` (after [data_inspect()]). Unknown mappings are never guessed.
#'
#' @param packages Package names.
#' @param lib.loc Library directories searched, in order, for each package.
#' @param id Collection ids (default: package names).
#' @param suffix `NULL` or one suffix per package (`NA` for none). Exposes each
#'   object also as `<object><suffix>`, e.g. `cds_main.reference`.
#' @param include `NULL`, or a named list (names are collection ids) of
#'   object/resource names to expose. A character vector is accepted when
#'   registering one package.
#' @param primary `NULL` or the id of the collection whose objects keep bare
#'   names when contested. Equivalent to calling [data_primary()].
#' @param replace Replace collections that are already registered.
#' @return The collection ids, invisibly.
#' @export
data_register <- function(packages, lib.loc = NULL, id = packages, suffix = NULL,
                          include = NULL, primary = NULL, replace = FALSE) {
  n <- length(packages)
  stopifnot(is.character(packages), n >= 1L, length(id) == n)
  id <- as.character(id)
  if (anyDuplicated(id)) pd_abort("Collection ids must be unique: ", quote_names(id[duplicated(id)]), ".")
  suffix <- if (is.null(suffix)) rep(NA_character_, n) else as.character(suffix)
  if (length(suffix) != n) pd_abort("`suffix` must have one element per package.")
  if (!is.null(include) && !is.list(include)) {
    if (n != 1L) pd_abort("`include` must be a named list when registering several packages.")
    include <- stats::setNames(list(include), id)
  }
  cand <- snapshot()
  taken <- intersect(id, names(cand$collections))
  if (length(taken) && !replace) {
    pd_abort("Collection(s) already registered: ", quote_names(taken),
             ". Use replace = TRUE or data_unregister() first.")
  }
  for (i in seq_len(n)) {
    cand$collections[[id[i]]] <- build_collection(packages[i], lib.loc, id[i],
                                                  suffix[i], include[[id[i]]])
  }
  if (!is.null(primary)) cand$primary <- primary
  validate_state(cand)
  commit_state(cand)
  invisible(id)
}

#' Unregister collections
#'
#' Removes the registration, its per-collection environment, its facade
#' bindings, and preferences or the primary designation that refer to it.
#' It does not delete files, close resources, or invalidate objects you
#' already hold. Work directories created by loaders are left in place (they
#' live in the session temporary directory).
#'
#' @param collections Collection ids.
#' @return The ids, invisibly.
#' @export
data_unregister <- function(collections) {
  cand <- snapshot()
  for (id in collections) get_collection(id)
  for (id in collections) {
    clear_env(cand$collections[[id]]$env)
    cand$collections[[id]] <- NULL
  }
  cand$preferences <- cand$preferences[!cand$preferences %in% collections]
  if (cand$primary %in% collections) cand$primary <- NA_character_
  cand$attached <- setdiff(cand$attached, collections)
  validate_state(cand)
  commit_state(cand)
  if (!length(.state$attached) && !is.na(facade_pos(.state$facade$env))) data_detach()
  invisible(collections)
}

#' List registered collections
#' @return A data frame, one row per collection. No data are touched.
#' @export
data_collections <- function() {
  cs <- .state$collections
  data.frame(
    id = names(cs),
    package = vapply(cs, function(x) x$package, ""),
    version = vapply(cs, function(x) x$version, ""),
    library = vapply(cs, function(x) x$library, ""),
    primary = names(cs) %in% .state$primary,
    suffix = vapply(cs, function(x) x$suffix, ""),
    attached = names(cs) %in% .state$attached,
    resources = vapply(cs, function(x) length(x$resources), 1L),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Primary collection and per-object preferences
#'
#' The primary collection is the explicit, user-chosen winner for contested bare
#' names; there is no implicit winner based on registration or attachment
#' order. A preference for a specific object overrides the primary. Neither
#' reads data.
#'
#' @param id Collection id, or `NULL` to clear. If missing, the current
#'   primary is returned.
#' @param name Object name(s).
#' @param collection Collection id(s) to prefer for `name`, or `NULL` to clear.
#' @return `data_primary()` returns the primary id (or `NA`); the others return
#'   their result invisibly / a data frame.
#' @export
data_primary <- function(id) {
  if (missing(id)) return(.state$primary)
  cand <- snapshot()
  cand$primary <- if (is.null(id)) NA_character_ else id
  validate_state(cand)
  commit_state(cand)
  invisible(.state$primary)
}

#' @rdname data_primary
#' @export
data_prefer <- function(name, collection) {
  cand <- snapshot()
  if (is.null(collection)) {
    cand$preferences <- cand$preferences[!names(cand$preferences) %in% name]
  } else {
    collection <- rep_len(collection, length(name))
    for (i in seq_along(name)) cand$preferences[[name[i]]] <- collection[i]
  }
  validate_state(cand)
  commit_state(cand)
  invisible(data_preferences())
}

#' @rdname data_primary
#' @export
data_preferences <- function() {
  p <- .state$preferences
  data.frame(name = names(p), collection = unname(p), row.names = NULL,
             stringsAsFactors = FALSE)
}

# Access ---------------------------------------------------------------------

#' Explicit access to data
#'
#' `data_get()` returns an object without the facade. With `collection`, `name`
#' is an original object name in that collection (qualified access). Without
#' it, `name` is resolved with the same rules as the facade (including suffix
#' aliases) and an unresolved ambiguity is an error naming the candidates.
#' `data_env()` returns the per-collection environment: active bindings for the
#' collection's exposed objects under their original names.
#'
#' Repeated and aliased access share one cached resource; the returned object is
#' the shared reference, **not** an independent copy. Modifying an environment
#' or reference-semantics object affects every alias.
#'
#' @param name Object name or alias.
#' @param collection Optional collection id.
#' @param id Collection id for `data_env()`.
#' @return The data object, or an environment.
#' @export
data_get <- function(name, collection = NULL) {
  stopifnot(is_scalar_string(name))
  res <- resolve_names(.state)
  if (!is.null(collection)) {
    co <- get_collection(collection)
    o <- co$objects[co$objects$exposed & !is.na(co$objects$object) & co$objects$object == name, ]
    if (!nrow(o)) pd_abort("Collection '", collection, "' does not expose an object '", name, "'.")
    return(get_object(collection, name))
  }
  hit <- res[res$name == name, , drop = FALSE]
  if (!nrow(hit)) pd_abort("No registered object named '", name, "'.", class = "projectData_not_found")
  if (any(hit$binding == "ambiguous")) pd_abort(ambiguity_message(name, res), class = "projectData_ambiguous")
  w <- hit[hit$binding == "bound", , drop = FALSE]
  get_object(w$collection[1], w$object[1])
}

#' @rdname data_get
#' @export
data_env <- function(id) get_collection(id)$env

#' Eagerly open resources, and retry failed ones
#'
#' `data_load()` opens the resources behind the selected objects (or every
#' resource of the selected collections) now, for predictable batch use. It
#' may use a lot of memory or time for large data. `data_retry()` resets
#' `failed` resources to `unopened` so that the next access tries again.
#'
#' @param collection Collection id(s); `NULL` means all.
#' @param names Optional object names to restrict to.
#' @param resource Resource id(s); `NULL` means all failed resources.
#' @return `data_load()` returns the catalog rows for the opened resources
#'   (invisibly); `data_retry()` returns the reset resource ids invisibly.
#' @export
data_load <- function(collection = NULL, names = NULL) {
  ids <- collection %||% names(.state$collections)
  for (id in ids) {
    co <- get_collection(id)
    o <- co$objects
    res <- if (is.null(names)) names(co$resources) else unique(o$resource[o$object %in% names])
    for (r in res) open_resource(co, co$resources[[r]])
  }
  invisible(data_catalog(ids))
}

#' @rdname data_load
#' @export
data_retry <- function(collection, resource = NULL) {
  co <- get_collection(collection)
  rs <- resource %||% names(co$resources)
  reset <- character()
  for (r in rs) {
    rec <- co$resources[[r]]
    if (is.null(rec)) pd_abort("Collection '", collection, "' has no resource '", r, "'.")
    if (rec$state == "failed") {
      rec$state <- "unopened"
      rec$error <- NULL
      reset <- c(reset, r)
    }
  }
  invisible(reset)
}

#' Deep inspection of a resource's contents
#'
#' Opens the resource through its loader and replaces its object mapping with
#' the objects actually produced (`mapping = "observed"`). This is the only way
#' to learn the contents of a resource whose mapping is `unknown`. It is
#' never run implicitly. For ordinary `.rda`/`.RData` datasets the file is
#' loaded into an isolated environment; `data/*.R` scripts are **executed**.
#' The opened resource is cached, so a later access does not read it again.
#'
#' @param collection Collection id.
#' @param resource Resource id(s); `NULL` means every resource whose mapping is
#'   `unknown`.
#' @return The updated catalog rows for the collection, invisibly.
#' @export
data_inspect <- function(collection, resource = NULL) {
  co <- get_collection(collection)
  rs <- resource %||% unique(co$objects$resource[co$objects$confidence == "unknown"])
  if (!length(rs)) return(invisible(data_catalog(collection)))
  for (r in rs) {
    rec <- co$resources[[r]]
    if (is.null(rec)) pd_abort("Collection '", collection, "' has no resource '", r, "'.")
    if (rec$state != "ready") {
      pd_warn("Deep inspection of resource '", r, "' loads it fully into an isolated ",
              "environment (and runs data scripts). This can use substantial memory and time.",
              class = "projectData_inspection")
    }
    open_resource(co, rec)
  }
  cand <- snapshot()
  new <- cand$collections[[collection]]
  keep <- new$objects[!new$objects$resource %in% rs, , drop = FALSE]
  add <- lapply(rs, function(r) {
    obs <- co$resources[[r]]$observed
    if (!length(obs)) pd_abort("Resource '", r, "' opened but produced no objects.")
    old <- new$objects[new$objects$resource == r, , drop = FALSE]
    data.frame(resource = r, object = obs, confidence = "observed",
               title = old$title[match(obs, old$object)],
               help_topic = old$help_topic[match(obs, old$object)],
               recorded_class = old$recorded_class[match(obs, old$object)],
               stringsAsFactors = FALSE)
  })
  tab <- rbind_list(c(list(keep[, setdiff(names(keep), "exposed")]), add), keep[0, setdiff(names(keep), "exposed")])
  new$objects <- apply_include(tab, new$include)
  cand$collections[[collection]] <- new
  validate_state(cand)
  commit_state(cand)
  invisible(data_catalog(collection))
}
