build_collection <- function(package, lib.loc, id, suffix, include) {
  inst <- resolve_installation(package, lib.loc)
  if (!is_scalar_string(id)) pd_abort("Collection ids must be non-empty strings.")
  if (!is.na(suffix) && !nzchar(suffix)) {
    pd_abort("`suffix` must be a non-empty string (use `primary` to keep bare names).")
  }
  context <- loader_context(id, package, inst$version, inst$library, inst$root)
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
       resources = resources, objects = objects, attached = FALSE)
}

# Reuses opened/cached resource records of a replaced, still unattached
# registration of the *same installation*, so that re-registering with other
# attach arguments (suffix, include) never makes a resource load twice. Objects
# mappings of reused records are kept too (e.g. after data_inspect()).
carry_resources <- function(new, old) {
  if (is.null(old) || isTRUE(old$attached) || !identical(old$identity, new$identity) ||
      !identical(old$root, new$root)) {
    return(new)
  }
  same <- vapply(names(new$resources), function(r) {
    !is.null(old$resources[[r]]) &&
      identical(old$resources[[r]]$descriptor, new$resources[[r]]$descriptor)
  }, NA)
  keep <- names(new$resources)[same]
  if (!length(keep)) return(new)
  new$resources[keep] <- old$resources[keep]
  cols <- setdiff(names(new$objects), "exposed")
  fresh <- new$objects[!new$objects$resource %in% keep, cols, drop = FALSE]
  kept <- old$objects[old$objects$resource %in% keep, cols, drop = FALSE]
  new$objects <- apply_include(rbind_list(list(fresh, kept), fresh[0, ]), new$include)
  new
}

# Builds collections and adds them to a candidate state; nothing is committed.
add_collections <- function(cand, packages, lib.loc, id, suffix, include, replace) {
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
  # `replace` is one flag or one per package.
  taken <- id[id %in% names(cand$collections) & !rep_len(replace, n)]
  if (length(taken)) {
    state <- vapply(taken, function(i) if (isTRUE(cand$collections[[i]]$attached)) "attached" else "registered", "")
    pd_abort("Collection(s) already ", paste(unique(state), collapse = "/"), ": ",
             paste0("'", taken, "' (", state, ")", collapse = ", "),
             ". Use replace = TRUE or data_detach() first.")
  }
  for (i in seq_len(n)) {
    cand$collections[[id[i]]] <- carry_resources(
      build_collection(packages[i], lib.loc, id[i], suffix[i], include[[id[i]]]),
      cand$collections[[id[i]]])
  }
  cand
}

#' List registered collections
#'
#' Collections are registered automatically, either by [data_attach()] (which
#' also attaches them) or by [data_get()] with `package` (which does not).
#' @return A data frame, one row per registered collection, with `attached`
#'   telling whether its names are exposed on the search path. No data are
#'   touched.
#' @export
data_collections <- function() {
  cs <- .state$collections
  data.frame(
    id = names(cs),
    package = vapply(cs, function(x) x$package, ""),
    version = vapply(cs, function(x) x$version, ""),
    library = vapply(cs, function(x) x$library, ""),
    attached = vapply(cs, function(x) isTRUE(x$attached), NA),
    primary = names(cs) %in% .state$primary,
    suffix = vapply(cs, function(x) x$suffix, ""),
    resources = vapply(cs, function(x) length(x$resources), 1L),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Primary collection and per-object preferences
#'
#' These apply to attached collections (registered collections that
#' [data_get()] created without attaching them take no part in bare-name
#' resolution). The primary collection is the explicit, user-chosen winner for contested bare
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

#' Get data objects explicitly
#'
#' Returns objects without exposing them on the search path, so it is the most
#' robust form in scripts and functions, and plays the role for projectData that
#' `package::object` plays for exported data. It never attaches anything and
#' never changes an attached collection.
#'
#' **Unqualified** (`package = NULL`, `collection = NULL`): `name` is resolved
#' exactly as a bare name is among the attached collections, including suffix
#' aliases, and an unresolved ambiguity is an error naming the candidates.
#'
#' **By collection id**: with `collection`, `name` is an original object name in
#' that registered collection (it also reaches objects that lost a name
#' conflict).
#'
#' **By package**: with `package`, the collection is registered automatically if
#' necessary, from `lib.loc` (default [.libPaths()]), and `name` is an original
#' object name in it. Registration reads package metadata only. Retrieval is
#' independent of primary, preferences and suffixes used for attached access.
#' If the package is already registered (attached or not) it is reused: the
#' installation recorded then stays in force, and `lib.loc` is only checked
#' against it, so a second installed copy is never chosen silently. If several
#' collections come from the package, say which with `collection`; `collection`
#' is also the id given to a newly registered collection (default: the package
#' name).
#'
#' `data_get()` **opens** the requested resource: it is not a metadata query
#' (see [data_query()] and [data_catalog()] for those). Resources not requested
#' stay unopened, but a resource that produces several objects loads all of
#' them. Give several names to open several objects in one call; a single name
#' returns the object, several return a named list.
#'
#' Explicit and attached access share one cached resource per collection,
#' whichever happens first. The returned object is the shared reference, **not**
#' an independent copy. Registration created by `data_get()` persists for the
#' session until [data_detach()] forgets it.
#'
#' @param name Object name(s) or alias(es).
#' @param collection Optional collection id (recycled over `name`).
#' @param package Optional installed package name (recycled over `name`).
#' @param lib.loc Library directories searched when `package` has to be
#'   registered; checked against the installation of an existing registration.
#' @return The object, or a named list for several names.
#' @examples
#' # Retrieve one object from an installed package without attaching it
#' head(data_get("mtcars", package = "datasets"))
#' @export
data_get <- function(name, collection = NULL, package = NULL, lib.loc = NULL) {
  stopifnot(is.character(name), length(name) >= 1L, !anyNA(name))
  n <- length(name)
  collection <- if (is.null(collection)) rep(NA_character_, n) else rep_len(collection, n)
  package <- if (is.null(package)) rep(NA_character_, n) else rep_len(as.character(package), n)
  # Register everything first so a bad package fails before any resource opens.
  ids <- collection
  for (i in which(!is.na(package))) {
    ids[i] <- ensure_registered(package[i], collection[i], lib.loc)
  }
  res <- if (anyNA(ids)) resolve_names(.state)
  vals <- lapply(seq_len(n), function(i) get_one(name[i], ids[i], res))
  if (n == 1L) vals[[1L]] else stats::setNames(vals, name)
}

# Finds or creates the registration for `package`, without attaching anything
# and without touching the facade. Returns the collection id. Metadata only.
ensure_registered <- function(package, id, lib.loc) {
  if (!is_scalar_string(package)) pd_abort("`package` must be a package name.")
  cs <- .state$collections
  given <- !is.na(id)
  from_pkg <- names(cs)[vapply(cs, function(co) identical(co$package, package), NA)]
  hit <- if (given) intersect(id, names(cs)) else from_pkg
  if (given && length(hit) && !identical(cs[[id]]$package, package)) {
    pd_abort("Collection id '", id, "' is already registered from package '", cs[[id]]$package,
             "', not '", package, "'. Choose another `collection`.")
  }
  if (!given && !length(hit) && package %in% names(cs)) {
    pd_abort("Collection id '", package, "' is already registered from package '",
             cs[[package]]$package, "'. Give `collection` an unused id for '", package, "'.")
  }
  if (!is.null(lib.loc) && length(hit)) {
    want <- resolve_installation(package, lib.loc)
    hit <- hit[vapply(hit, function(h) identical(cs[[h]]$root, want$root), NA)]
    if (!length(hit)) {
      pd_abort("Package '", package, "' is registered from another installation than the one ",
               "found in `lib.loc` (", want$root, "). Use another `collection` id, or ",
               "data_detach() the existing registration first.")
    }
  }
  if (length(hit) > 1L) {
    pd_abort("Several collections come from package '", package, "' (", quote_names(hit),
             "); give `collection`.", class = "projectData_ambiguous")
  }
  if (length(hit)) return(hit)
  new_id <- if (given) id else package
  cand <- snapshot()
  cand$collections[[new_id]] <- build_collection(package, lib.loc, new_id, NA_character_, NULL)
  validate_state(cand)
  # Commit without sync_all(): registration never rebinds the facade.
  assign_state(cand)
  new_id
}

get_one <- function(name, collection, res) {
  if (!is.na(collection)) {
    co <- get_collection(collection)
    o <- co$objects[co$objects$exposed & !is.na(co$objects$object) & co$objects$object == name, ]
    if (!nrow(o)) pd_abort("Collection '", collection, "' does not expose an object '", name, "'.")
    return(get_object(collection, name))
  }
  hit <- res[res$name == name, , drop = FALSE]
  if (!nrow(hit)) {
    pd_abort("No attached object named '", name, "'. Use `package` or `collection` to ",
             "retrieve from a collection that is not attached.", class = "projectData_not_found")
  }
  if (any(hit$binding == "ambiguous")) pd_abort(ambiguity_message(name, res), class = "projectData_ambiguous")
  w <- hit[hit$binding == "bound", , drop = FALSE]
  get_object(w$collection[1], w$object[1])
}

#' Retry a failed resource
#'
#' Resets `failed` resources to `unopened` so that the next access tries
#' again. A failure is otherwise sticky, so a broken backing file is not
#' re-read on every access.
#'
#' @param collection Collection id.
#' @param resource Resource id(s); `NULL` means all failed resources.
#' @return The reset resource ids, invisibly.
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
#' the objects actually produced (`mapping = "verified"`; `origin` is unchanged). This is the only way
#' to learn the contents of a resource whose mapping is `unknown`. It is
#' never run implicitly. For ordinary `.rda`/`.RData` datasets the file is
#' loaded into an isolated environment; `data/*.R` scripts are **executed**.
#' The opened resource is cached, so a later access does not read it again.
#'
#' The mapping is replaced even when none of the recorded names were produced
#' (for example an index entry `example` for a file that actually holds
#' `sample_metadata`) or only some were. Ordinary access ([data_get()]) stays
#' strict and errors on an absent name, pointing here. Inspection only
#' replaces the mapping after the loader succeeded: a genuine opening failure
#' leaves the mapping untouched (and `data_retry()` applies). The new mapping
#' is committed through the usual validated catalog update (if validation fails,
#' nothing changes). A newly observed name that collides with another
#' collection is handled by the normal conflict rules: it is `ambiguous` unless
#' a primary, preference or suffix alias resolves it. Names
#' that disappear from the mapping are no longer bound; names that appear are
#' bound or aliased as for any other object. Help topics are carried over only
#' for names that were already recorded.
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
