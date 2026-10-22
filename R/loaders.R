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
#' `adapter` (`list(package=, export=)`, set by manifests).
#'
#' `context` is a list built by one helper for validation and opening, so the
#' shared fields are identical: `collection` (collection id), `package`,
#' `version`, `library` (the library holding the installed package), `root`
#' (the installation root, the same path passed to `discover` and `validate`)
#' and `resource` (the resource id). During discovery `resource` is `NA`.
#' During a standalone [data_manifest_validate()] call `collection` is `NA`.
#' The only information available solely at opening is the `paths` argument of
#' `open` (resolved `source` and `work_dir`).
#'
#' `validate` runs without opening: when a collection is registered (if the
#' loader is already registered) and again before every uncached opening
#' attempt, against the loader selected for that attempt. It must only inspect
#' metadata and file paths; it must not restore the dataset or read backing
#' matrices.
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
  id <- if (is_scalar_string(loader$id)) loader$id else "<invalid id>"
  bad <- function(...) pd_abort("Loader '", id, "' is malformed: ", ...)
  if (!identical(loader$contract, loader_contract_version)) {
    pd_abort("Loader '", id, "' uses contract version ", loader$contract,
             "; this projectData supports version ", loader_contract_version, ".")
  }
  if (!is_scalar_string(loader$id) || !grepl(loader_id_pattern, loader$id)) {
    bad("the id must be namespaced like 'pkg::name'.")
  }
  if (!is.function(loader$open)) bad("`open` must be a function.")
  if (!is.null(loader$discover) && !is.function(loader$discover)) {
    bad("`discover` must be a function or NULL.")
  }
  if (!is.null(loader$validate) && !is.function(loader$validate)) {
    bad("`validate` must be a function or NULL.")
  }
  if (!is_scalar_string(loader$storage) ||
      !loader$storage %in% c("memory", "disk-backed", "unknown")) {
    bad("`storage` must be 'memory', 'disk-backed' or 'unknown'.")
  }
  if (!is.logical(loader$needs_work_dir) || length(loader$needs_work_dir) != 1L ||
      is.na(loader$needs_work_dir)) {
    bad("`needs_work_dir` must be TRUE or FALSE.")
  }
  invisible(loader)
}

# Structure of a whole adapter declaration. Nothing is called.
check_declaration <- function(loaders) {
  if (inherits(loaders, "projectData_loader")) loaders <- list(loaders)
  if (!is.list(loaders) || inherits(loaders, "data.frame")) {
    pd_abort("An adapter must return a loader or a list of loaders created by data_loader().")
  }
  if (!length(loaders)) pd_abort("An adapter declared no loaders.")
  lapply(loaders, check_loader)
  ids <- vapply(loaders, function(l) l$id, "")
  if (anyDuplicated(ids)) {
    pd_abort("An adapter declared loader id(s) more than once: ",
             quote_names(unique(ids[duplicated(ids)])), ". Nothing was registered.")
  }
  loaders
}

# Context shared by validation and opening.
loader_context <- function(collection, package, version, library, root,
                           resource = NA_character_) {
  list(collection = collection, package = package, version = version,
       library = library, root = root, resource = resource)
}

# Context for a standalone root (no collection is involved).
root_context <- function(root) {
  meta <- file.path(root, "Meta", "package.rds")
  version <- if (file.exists(meta)) installed_description(root)[["Version"]] else NA_character_
  loader_context(NA_character_, basename(root), version, dirname(root), root)
}

# Returns NULL when the descriptor is acceptable, otherwise a message. Only
# metadata and paths are available to the validator.
validate_with_loader <- function(loader, d, root, context) {
  if (is.null(loader$validate)) return(NULL)
  res <- tryCatch(loader$validate(d, root, context),
                  error = function(e) paste0("validator failed: ", conditionMessage(e)))
  if (isTRUE(res)) return(NULL)
  if (is.character(res) && length(res)) return(paste(res, collapse = "; "))
  "loader rejected the descriptor"
}

#' Register, replace, remove, and list loaders
#'
#' Loaders live in the current R session only. Registering a loader never runs
#' it; discovery runs when a collection is attached with
#' [data_attach()] or [data_query()]. Registration is atomic: when several
#' loaders are registered together (see [data_loader_adapter()]) and any one
#' is rejected, the registry is left unchanged.
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
  invisible(add_loaders(list(loader), replace = replace, builtin = builtin))
}

# Prepares the complete new registry first and commits it with one assignment.
add_loaders <- function(loaders, replace, builtin) {
  loaders <- check_declaration(loaders)
  ids <- vapply(loaders, function(l) l$id, "")
  if (!builtin && any(startsWith(ids, builtin_prefix))) {
    pd_abort("The '", builtin_prefix, "' loader prefix is reserved for built-in loaders.")
  }
  taken <- ids[ids %in% names(.state$loaders)]
  if (length(taken) && !replace) {
    pd_abort("Loader ", quote_names(taken), " is already registered; use replace = TRUE ",
             "to replace it.", if (length(ids) > 1L) " Nothing was registered." else "")
  }
  registry <- .state$loaders
  registry[ids] <- loaders
  .state$loaders <- registry
  ids
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
#' no other packages are scanned. The export is called once; its result is
#' fully checked (every element is a valid loader, ids are unique within the
#' declaration, and no id collides with the registry under the `replace`
#' policy) before any loader is registered, so registration either succeeds
#' completely or changes nothing. No discover, validate or open function is
#' called while checking.
#'
#' @param package Adapter package name.
#' @param export Name of an exported function returning loader(s).
#' @param replace Passed to [data_loader_add()].
#' @return The registered ids, invisibly.
#' @export
data_loader_adapter <- function(package, export = "projectData_loaders",
                                replace = FALSE) {
  loaders <- fetch_adapter_loaders(package, export)
  invisible(add_loaders(loaders, replace = replace, builtin = FALSE))
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
  check_declaration(get(export, envir = ns)())
}

find_loader <- function(id) .state$loaders[[id]]

# Built-in loaders -----------------------------------------------------------

register_builtin_loaders <- function() {
  add_loader(
    data_loader(
      "projectData::dataset",
      open = function(descriptor, paths, context) {
        # baseenv() lets data/*.R scripts find base functions and `<-` without
        # reaching the global workspace or attached packages.
        env <- new.env(parent = baseenv())
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
