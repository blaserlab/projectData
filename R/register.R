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
       resources = resources, objects = objects)
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
  taken <- intersect(id, names(cand$collections))
  if (length(taken) && !replace) {
    pd_abort("Collection(s) already attached: ", quote_names(taken),
             ". Use replace = TRUE or data_detach() first.")
  }
  for (i in seq_len(n)) {
    cand$collections[[id[i]]] <- build_collection(packages[i], lib.loc, id[i],
                                                  suffix[i], include[[id[i]]])
  }
  cand
}

#' List attached collections
#' @return A data frame, one row per attached collection. No data are touched.
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
    resources = vapply(cs, function(x) length(x$resources), 1L),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Primary collection and per-object preferences
#'
#' These apply to attached collections. The primary collection is the explicit, user-chosen winner for contested bare
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
#' Returns objects without relying on bare names on the search path, which is
#' the most robust form in scripts and functions. The collections must be
#' attached with [data_attach()]. With `collection`, `name` is an original
#' object name in that collection (qualified access, which also reaches
#' objects that lost a name conflict). Without it, `name` is resolved exactly as
#' a bare name is, including suffix aliases, and an unresolved ambiguity is an
#' error naming the candidates.
#'
#' Give several names to open several objects in one call, for example to open
#' all large objects at the start of a batch job: the first-access messages
#' then appear together, before the rest of the analysis. A single name returns
#' the object; several names return a named list.
#'
#' Repeated and aliased access share one cached resource; the returned object is
#' the shared reference, **not** an independent copy. Modifying an environment
#' or reference-semantics object affects every alias.
#'
#' @param name Object name(s) or alias(es).
#' @param collection Optional collection id (recycled over `name`).
#' @return The object, or a named list for several names.
#' @export
data_get <- function(name, collection = NULL) {
  stopifnot(is.character(name), length(name) >= 1L, !anyNA(name))
  collection <- if (is.null(collection)) rep(NA_character_, length(name)) else
    rep_len(collection, length(name))
  res <- resolve_names(.state)
  vals <- lapply(seq_along(name), function(i) get_one(name[i], collection[i], res))
  if (length(name) == 1L) vals[[1L]] else stats::setNames(vals, name)
}

get_one <- function(name, collection, res) {
  if (!is.na(collection)) {
    co <- get_collection(collection)
    o <- co$objects[co$objects$exposed & !is.na(co$objects$object) & co$objects$object == name, ]
    if (!nrow(o)) pd_abort("Collection '", collection, "' does not expose an object '", name, "'.")
    return(get_object(collection, name))
  }
  hit <- res[res$name == name, , drop = FALSE]
  if (!nrow(hit)) pd_abort("No attached object named '", name, "'.", class = "projectData_not_found")
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
