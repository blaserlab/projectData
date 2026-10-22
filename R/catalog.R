#' Catalog, info, and help
#'
#' The catalog is a plain data frame (class `projectData_catalog`) built purely
#' from recorded metadata and resource state. Building or printing it never
#' opens a resource and never calls [get()], [class()], [dim()] or
#' [object.size()] on data. One row per exposed name (original and alias); a
#' contested name appears once per candidate collection.
#'
#' Columns: `collection`, `package`, `version`, `library`, `root`, `resource`
#' (the loadable unit), `object`, `exposed_name`, `kind` (`original`/`alias`),
#' `binding` (`bound`: the bare name resolves to this row; `not_bound`: lost to
#' a preference or primary; `ambiguous`: unresolved conflict; `excluded`: not
#' selected by `include`; `unmapped`: object unknown), `loader`, `source`,
#' `help_topic`, `title`, `mapping`, `state`, `storage`, `recorded_class`.
#'
#' `state` is one of `unopened`, `opening`, `ready`, `failed`,
#' `unavailable-loader`. `ready` means the loader returned; for `storage` =
#' `disk-backed` this does not mean the data are in memory. `recorded_class`
#' is whatever the creator declared; it is **recorded, not observed**.
#' `mapping` becomes `verified` for a ready resource whose observed objects
#' include all recorded ones.
#'
#' `data_info()` returns the catalog rows for a name. `data_help()` explicitly
#' opens the help page of the *original* topic in the source package
#' (aliases lead to the same topic); it is separate from inspection.
#'
#' @param collection Collection id(s); `NULL` means all.
#' @param name Original object name or exposed name.
#' @return A `projectData_catalog` data frame; `data_help()` returns the help
#'   object from [utils::help()].
#' @export
data_catalog <- function(collection = NULL) {
  st <- .state
  res <- resolve_names(st)
  cols <- c("collection", "package", "version", "library", "root", "resource",
            "object", "exposed_name", "kind", "binding", "loader", "source",
            "help_topic", "title", "mapping", "state", "storage", "recorded_class")
  parts <- lapply(st$collections, function(co) {
    if (!is.null(collection) && !co$id %in% collection) return(NULL)
    o <- co$objects
    if (!nrow(o)) return(NULL)
    recs <- co$resources[o$resource]
    state <- vapply(recs, resource_state, "")
    storage <- vapply(recs, function(r) {
      l <- find_loader(r$loader)
      if (is.null(l)) "unknown" else l$storage
    }, "")
    mapping <- o$confidence
    for (i in which(state == "ready" & !is.na(o$object))) {
      if (o$object[i] %in% recs[[i]]$observed) mapping[i] <- "verified"
    }
    base <- data.frame(
      collection = co$id, package = co$package, version = co$version,
      library = co$library, root = co$root, resource = o$resource, object = o$object,
      loader = vapply(recs, function(r) r$loader, ""),
      source = vapply(recs, function(r) r$source, ""),
      help_topic = o$help_topic, title = o$title, mapping = mapping,
      state = state, storage = storage, recorded_class = o$recorded_class,
      exposed = o$exposed, stringsAsFactors = FALSE, row.names = NULL
    )
    mine <- res[res$collection == co$id, , drop = FALSE]
    m <- match(mine$object, base$object)
    named <- base[m, , drop = FALSE]
    named$exposed_name <- mine$name
    named$kind <- mine$kind
    named$binding <- mine$binding
    rest <- base[is.na(base$object) | !base$exposed, , drop = FALSE]
    if (nrow(rest)) {
      rest$exposed_name <- NA_character_
      rest$kind <- NA_character_
      rest$binding <- ifelse(is.na(rest$object), "unmapped", "excluded")
    }
    rbind(named, rest)
  })
  out <- rbind_list(parts, stats::setNames(as.data.frame(
    matrix(character(), 0, length(cols)), stringsAsFactors = FALSE), cols))
  if (nrow(out)) out <- out[, cols, drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("projectData_catalog", "data.frame")
  out
}

#' @export
print.projectData_catalog <- function(x, ...) {
  cat("<projectData catalog>: ", nrow(x), " row(s); metadata only, no data opened.\n", sep = "")
  show <- c("collection", "exposed_name", "object", "resource", "binding", "mapping", "state")
  y <- unclass_catalog(x)
  y <- y[, intersect(show, names(y)), drop = FALSE]
  print(y, row.names = FALSE, ...)
  invisible(x)
}

unclass_catalog <- function(x) {
  class(x) <- "data.frame"
  x
}

#' @export
`[.projectData_catalog` <- function(x, ...) {
  out <- NextMethod()
  if (is.data.frame(out) && all(c("collection", "exposed_name") %in% names(out))) {
    class(out) <- c("projectData_catalog", "data.frame")
  }
  out
}

#' @rdname data_catalog
#' @export
data_info <- function(name, collection = NULL) {
  cat <- data_catalog(collection)
  hit <- cat[!is.na(cat$exposed_name) & cat$exposed_name == name |
               !is.na(cat$object) & cat$object == name, ]
  if (!nrow(hit)) pd_abort("No catalog entry for '", name, "'.", class = "projectData_not_found")
  hit
}

#' @rdname data_catalog
#' @export
data_help <- function(name, collection = NULL) {
  hit <- data_info(name, collection)
  key <- unique(hit[c("collection", "object")])
  if (nrow(key) > 1L) {
    pd_abort("'", name, "' exists in several collections (", quote_names(key$collection),
             "); give `collection`.", class = "projectData_ambiguous")
  }
  topic <- hit$help_topic[1]
  if (is.na(topic)) {
    pd_abort("No help topic is recorded for '", hit$object[1], "' in collection '",
             hit$collection[1], "' (the object appears undocumented).")
  }
  # help() captures unevaluated arguments; do.call passes literal strings.
  do.call(utils::help, list(topic, package = hit$package[1], lib.loc = hit$library[1]))
}
