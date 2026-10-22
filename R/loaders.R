loader_contract_version <- 1L
loader_id_pattern <- "^[A-Za-z][A-Za-z0-9.]*::[A-Za-z][A-Za-z0-9._-]*$"
builtin_prefix <- "projectData::"

descriptor_fields <- c("resource", "loader", "source", "objects", "options",
                       "title", "help_topic", "recorded_class", "adapter")

#' Create a loader definition (loader contract version 1)
#'
#' A loader separates three steps. All three receive resolved local
#' information only, and only `open` may read data values.
#'
#' * `discover(root, context)`: inspect a collection root and return a list of
#'   descriptors (or `NULL`) without reading data values.
#' * `validate(descriptor, root, context)`: check a descriptor and its files
#'   without opening them. Return `TRUE`, or signal an error / return a
#'   character message to reject it.
#' * `open(descriptor, paths, context)`: restore the resource and return a
#'   **named list** of the objects it produces. `paths` is a list with `root`,
#'   `source` (absolute path or `NA`) and `work_dir` (a path unique to this
#'   open attempt when `needs_work_dir = TRUE`, otherwise `NULL`).
#'
#' A descriptor is a list with fields `resource` (id, required), `loader`
#' (id; defaults to the discovering loader), `source` (path relative to the
#' root, or `NA`), `objects` (character vector of object names the resource
#' produces, or `NULL` when unknown), `options` (named list), `title`,
#' `help_topic`, `recorded_class` (recorded, not observed; optional), and
#' `adapter` (`list(package=, export=)`, set by manifests). `context` is a list
#' with `collection`, `package`, `version`, `library`, `resource`.
#'
#' Loader ids must be namespaced (`"pkg::name"`); the `projectData::` prefix is
#' reserved for built-in loaders. Installed files are read-only: an adapter
#' needing writable files must use `paths$work_dir`.
#'
#' @param id Namespaced loader id.
#' @param open Function `(descriptor, paths, context)` returning a named list.
#' @param discover,validate Optional functions as described above.
#' @param storage `"memory"`, `"disk-backed"` or `"unknown"`. Reported in the
#'   catalog; a ready disk-backed object is not necessarily in memory.
#' @param needs_work_dir Allocate a writable work directory for each open.
#' @return An object of class `projectData_loader`.
#' @seealso [data_loader_add()], [data_loader_adapter()]
#' @export
data_loader <- function(id, open, discover = NULL, validate = NULL,
                        storage = c("memory", "disk-backed", "unknown"),
                        needs_work_dir = FALSE) {
  storage <- match.arg(storage)
  if (!is_scalar_string(id) || !grepl(loader_id_pattern, id)) {
    pd_abort("Loader id must be namespaced like 'pkg::name'; got ",
             deparse(id), ".")
  }
  stopifnot(is.function(open), is.null(discover) || is.function(discover),
            is.null(validate) || is.function(validate),
            is.logical(needs_work_dir), length(needs_work_dir) == 1L)
  structure(
    list(id = id, contract = loader_contract_version, open = open,
         discover = discover, validate = validate, storage = storage,
         needs_work_dir = needs_work_dir),
    class = "projectData_loader"
  )
}

#' @export
print.projectData_loader <- function(x, ...) {
  cat("<projectData_loader> ", x$id, " (contract ", x$contract, ", storage: ",
      x$storage, ", discovers: ", !is.null(x$discover), ")\n", sep = "")
  invisible(x)
}

check_loader <- function(loader) {
  if (!inherits(loader, "projectData_loader")) {
    pd_abort("Expected a loader created by data_loader().")
  }
  if (!identical(loader$contract, loader_contract_version)) {
    pd_abort("Loader '", loader$id, "' uses contract version ", loader$contract,
             "; this projectData supports version ", loader_contract_version, ".")
  }
  invisible(loader)
}

#' Register, replace, remove, and list loaders
#'
#' Loaders live in the current R session only. Registering a loader never runs
#' it; discovery runs when a collection is attached with
#' [data_attach()] or [data_query()].
#'
#' Replacing a loader (`replace = TRUE`) affects resources opened afterwards.
#' Resources that are already `ready` keep their cached objects. Removing a
#' loader leaves ready objects untouched, and marks unopened resources that
#' need it as `unavailable-loader`. Built-in loaders cannot be removed or
#' replaced.
#'
#' @param loader A [data_loader()] object.
#' @param replace Allow replacing a loader with the same id.
#' @param id Loader id.
#' @return `data_loader_add()` and `data_loader_remove()` return the id
#'   invisibly; `data_loaders()` returns a data frame.
#' @export
data_loader_add <- function(loader, replace = FALSE) {
  check_loader(loader)
  add_loader(loader, replace = replace, builtin = FALSE)
}

add_loader <- function(loader, replace, builtin) {
  id <- loader$id
  if (!builtin && startsWith(id, builtin_prefix)) {
    pd_abort("The '", builtin_prefix, "' loader prefix is reserved for built-in loaders.")
  }
  if (!is.null(.state$loaders[[id]]) && !replace) {
    pd_abort("Loader '", id, "' is already registered; use replace = TRUE to replace it.")
  }
  .state$loaders[[id]] <- loader
  invisible(id)
}

#' @rdname data_loader_add
#' @export
data_loader_remove <- function(id) {
  if (startsWith(id, builtin_prefix)) pd_abort("Built-in loaders cannot be removed.")
  if (is.null(.state$loaders[[id]])) pd_abort("No loader '", id, "' is registered.")
  .state$loaders[[id]] <- NULL
  invisible(id)
}

#' @rdname data_loader_add
#' @export
data_loaders <- function() {
  l <- .state$loaders
  data.frame(
    id = names(l),
    storage = vapply(l, function(x) x$storage, character(1)),
    discovers = vapply(l, function(x) !is.null(x$discover), logical(1)),
    builtin = startsWith(names(l), builtin_prefix),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Register loaders from an adapter package
#'
#' Loads the adapter's namespace (without attaching it), calls its exported
#' function `export` (which takes no arguments), and registers the returned
#' [data_loader()] or list of loaders. Nothing is installed automatically and
#' no other packages are scanned.
#'
#' @param package Adapter package name.
#' @param export Name of an exported function returning loader(s).
#' @param replace Passed to [data_loader_add()].
#' @return The registered ids, invisibly.
#' @export
data_loader_adapter <- function(package, export = "projectData_loaders",
                                replace = FALSE) {
  loaders <- fetch_adapter_loaders(package, export)
  ids <- vapply(loaders, function(l) data_loader_add(l, replace = replace),
                character(1))
  invisible(ids)
}

fetch_adapter_loaders <- function(package, export) {
  if (!requireNamespace(package, quietly = TRUE)) {
    pd_abort("Adapter package '", package, "' is not installed. projectData ",
             "never installs packages automatically; install it and retry.",
             class = "projectData_loader_unavailable")
  }
  ns <- asNamespace(package)
  if (!export %in% getNamespaceExports(ns)) {
    pd_abort("Adapter package '", package, "' does not export '", export, "'.",
             class = "projectData_loader_unavailable")
  }
  out <- get(export, envir = ns)()
  if (inherits(out, "projectData_loader")) out <- list(out)
  lapply(out, check_loader)
}

find_loader <- function(id) .state$loaders[[id]]

# Built-in loaders -----------------------------------------------------------

register_builtin_loaders <- function() {
  add_loader(
    data_loader(
      "projectData::dataset",
      open = function(descriptor, paths, context) {
        env <- new.env(parent = emptyenv())
        utils::data(list = descriptor$resource, package = context$package,
                    lib.loc = context$library, envir = env, overwrite = TRUE)
        mget(sort(ls(env, all.names = TRUE)), envir = env)
      }
    ),
    replace = TRUE, builtin = TRUE
  )
  add_loader(
    data_loader(
      "projectData::rds",
      open = function(descriptor, paths, context) {
        obj <- readRDS(paths$source)
        stats::setNames(list(obj), descriptor$objects)
      },
      validate = function(descriptor, root, context) {
        if (is.na(descriptor$source)) return("an rds resource needs a Source file")
        if (length(descriptor$objects) != 1L) {
          return("an rds resource must declare exactly one object")
        }
        TRUE
      }
    ),
    replace = TRUE, builtin = TRUE
  )
}
