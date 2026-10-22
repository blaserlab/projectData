manifest_default_export <- "projectData_loaders"

#' Draft manifest entries from registered adapters
#'
#' Runs the `discover` step of registered loaders against a collection root and
#' returns the descriptors they claim, ready to review, edit and pass to
#' [data_manifest_write()]. Discovery uses metadata only: no resource is
#' opened, no data are read and nothing is written.
#'
#' Adapters must already be registered ([data_loader_add()] or
#' [data_loader_adapter()]); this function never loads or registers one. It
#' fails if no registered loader can discover resources, or if a loader named in
#' `loaders` is not registered.
#'
#' Resources that a manifest would have to decide are left out of the result
#' and reported instead, with a `projectData_draft_conflict` warning and the
#' `conflicts` attribute (a data frame with `resource`, `loader`, `reason`):
#'
#' * a resource claimed more than once (by several loaders, or twice by one),
#' * when `loaders` is `NULL`, a claim on a resource of the standard data
#'   inventory (`data/`), which the built-in reader keeps unless a manifest
#'   says otherwise.
#'
#' Naming the winning loader in `loaders` resolves both: only those loaders
#' are asked, and a claim on a standard dataset is then treated as a deliberate
#' reassignment and kept.
#'
#' Descriptors do not record which package supplies the loader. To let
#' projectData load an adapter on first access, set `AdapterPackage` (and
#' optionally `AdapterExport`) yourself, e.g.
#' `d$adapter <- list(package = "myAdapter")`.
#'
#' @param root Collection root to inspect: the installed package directory, or
#'   `inst/` in a source tree (where `Source` paths such as `extdata/x` resolve).
#' @param loaders Optional character vector of registered loader ids to use.
#'   Defaults to every registered loader that can discover.
#' @return A list of descriptors, with a `conflicts` attribute.
#' @seealso [data_manifest_write()], [data_manifest_validate()]
#' @export
data_manifest_draft <- function(root, loaders = NULL) {
  if (!is_scalar_string(root) || !dir.exists(root)) {
    pd_abort("`root` must be an existing directory.")
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  registry <- .state$loaders
  can_discover <- vapply(registry, function(l) !is.null(l$discover), logical(1))
  if (is.null(loaders)) {
    ids <- sort(setdiff(names(registry)[can_discover], names(registry)[startsWith(names(registry), builtin_prefix)]))
  } else {
    if (!is.character(loaders) || anyNA(loaders)) pd_abort("`loaders` must be a character vector of loader ids.")
    ids <- sort(unique(loaders))
    missing <- setdiff(ids, names(registry))
    if (length(missing)) {
      pd_abort("Loader(s) ", quote_names(missing), " are not registered. Register the ",
               "adapter first with data_loader_adapter() or data_loader_add().",
               class = "projectData_loader_unavailable")
    }
    no_discover <- ids[!can_discover[ids]]
    if (length(no_discover)) {
      pd_abort("Loader(s) ", quote_names(no_discover), " cannot discover resources.",
               class = "projectData_no_loaders")
    }
  }
  if (!length(ids)) {
    pd_abort("No registered loader can discover resources. Register an adapter first ",
             "with data_loader_adapter() or data_loader_add().",
             class = "projectData_no_loaders")
  }

  context <- root_context(root)
  claims <- list()
  for (id in ids) {
    ds <- registry[[id]]$discover(root, context)
    if (is.null(ds)) next
    if (!is.list(ds)) pd_abort("Discovery by loader '", id, "' must return a list of descriptors.")
    for (d in ds) {
      claims[[length(claims) + 1L]] <-
        normalize_descriptor(d, default_loader = id, origin = paste0("adapter:", id))
    }
  }
  claim_ids <- vapply(claims, function(d) d$resource, "")
  claim_loaders <- vapply(claims, function(d) d$loader, "")

  ambiguous <- claim_ids %in% claim_ids[duplicated(claim_ids)]
  standard <- rep(FALSE, length(claims))
  if (is.null(loaders) && length(claims)) {
    inventory <- withCallingHandlers(
      inventory_datasets(root),
      projectData_unsupported_format = function(w) invokeRestart("muffleWarning")
    )
    standard <- claim_ids %in% vapply(inventory, function(d) d$resource, "")
  }
  keep <- !ambiguous & !standard

  conflicts <- data.frame(
    resource = claim_ids[!keep], loader = claim_loaders[!keep],
    reason = ifelse(ambiguous, "claimed more than once",
                    "standard dataset kept by the built-in reader")[!keep],
    stringsAsFactors = FALSE
  )
  conflicts <- conflicts[order(conflicts$resource, conflicts$loader), , drop = FALSE]
  rownames(conflicts) <- NULL

  kept <- claims[keep]
  kept <- kept[order(claim_ids[keep])]
  for (d in kept) check_descriptor(d, root, context)

  if (nrow(conflicts)) {
    pd_warn("Left out of the draft; a manifest entry must decide: ",
            paste0(conflicts$resource, " (", conflicts$loader, ": ", conflicts$reason, ")",
                   collapse = ", "),
            ". Re-run with `loaders =` naming the loader that should handle them.",
            class = "projectData_draft_conflict")
  }
  structure(kept, conflicts = conflicts)
}

#' Write a projectData manifest
#'
#' Serializes descriptors (see [data_loader()] for their fields, or start from
#' [data_manifest_draft()]) to the manifest format read by
#' [data_manifest_validate()]. The manifest is data only. Nothing is opened and
#' no resource is read.
#'
#' Before anything is written, every descriptor is validated exactly as
#' [data_manifest_validate()] would (relative `Source` that exists below
#' `root`, namespaced loader id, unique object names). If the loader is
#' registered and has a `validate` function, that is called too. A loader that
#' is not registered cannot validate: the descriptor is still checked
#' structurally, the skip is recorded in the result and reported with a
#' `projectData_validation_skipped` message.
#'
#' A manifest can hold less than a descriptor can. These are errors, not
#' silent changes:
#'
#' * `title`, `help_topic` and `recorded_class` are one value per resource; a
#'   per-object vector is accepted only if all its values are the same.
#' * `options` must be a named list of non-empty, single character values. Keys
#'   cannot contain `=` or `;`, values cannot contain `;`.
#' * Object names cannot contain commas. No field may contain a line break or
#'   leading or trailing whitespace.
#' * Two resources cannot expose the same object name, and a resource can be
#'   declared only once.
#'
#' The written text is read back and must reproduce the descriptors exactly
#' before the file is put in place; it is written to a temporary file in the
#' target directory and then renamed, so a failure never leaves a partial or
#' half-replaced manifest.
#'
#' The file is never written inside an installed package, and an existing
#' file is replaced only with `overwrite = TRUE`. The conventional location is
#' `inst/projectData/manifest.dcf` in the package source. Existing manifests are
#' replaced as a whole, not merged.
#'
#' @param descriptors A list of descriptors (or one descriptor).
#' @param file Where to write. There is no default.
#' @param root Root against which `Source` paths are checked. Defaults to two
#'   directories above `file`, as in [data_manifest_validate()]; for
#'   `inst/projectData/manifest.dcf` that is `inst/`.
#' @param overwrite Replace an existing file.
#' @return Invisibly, a data frame with one row per resource and a
#'   `loader_validation` column (`"ran"`, or `"skipped: ..."` with the reason).
#'   The file path is in the `file` attribute.
#' @seealso [data_manifest_draft()], [data_manifest_validate()]
#' @export
data_manifest_write <- function(descriptors, file, root = dirname(dirname(file)),
                                overwrite = FALSE) {
  if (!is_scalar_string(file)) pd_abort("`file` must be a single path.")
  if (!is_scalar_string(root) || !dir.exists(root)) pd_abort("`root` must be an existing directory.")
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    pd_abort("`overwrite` must be TRUE or FALSE.")
  }
  if (is.list(descriptors) && is_scalar_string(descriptors$resource)) {
    descriptors <- list(descriptors)
  }
  if (!is.list(descriptors) || inherits(descriptors, "data.frame")) {
    pd_abort("`descriptors` must be a list of descriptors.")
  }
  if (inside_installed_package(dirname(file))) {
    pd_abort("Refusing to write '", file, "': it is inside an installed package, ",
             "which projectData treats as read-only. Write to the package source ",
             "(inst/projectData/manifest.dcf).")
  }
  if (file.exists(file) && !overwrite) {
    pd_abort("'", file, "' already exists; use overwrite = TRUE to replace it.")
  }

  descriptors <- unname(lapply(descriptors, prepare_manifest_descriptor))
  ids <- vapply(descriptors, function(d) d$resource, "")
  if (anyDuplicated(ids)) {
    pd_abort("Cannot write manifest: resource(s) ", quote_names(unique(ids[duplicated(ids)])),
             " declared more than once.", class = "projectData_manifest_error")
  }
  seen <- character()
  for (d in descriptors) {
    clash <- intersect(d$objects, seen)
    if (length(clash)) {
      pd_abort("Cannot write manifest: object(s) ", quote_names(clash), " produced by more ",
               "than one resource.", class = "projectData_duplicate_object")
    }
    seen <- c(seen, d$objects)
  }

  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  context <- root_context(root)
  status <- vapply(descriptors, function(d) {
    l <- find_loader(d$loader)
    if (is.null(l)) "skipped: loader not registered"
    else if (is.null(l$validate)) "skipped: loader has no validate function"
    else "ran"
  }, "")
  for (d in descriptors) check_descriptor(d, root, context)

  lines <- c("ManifestVersion: 1", unlist(lapply(descriptors, function(d) c("", manifest_record(d)))))

  dir <- dirname(file)
  created <- !dir.exists(dir)
  if (created) dir.create(dir, recursive = TRUE)
  tmp <- tempfile(".manifest-", tmpdir = dir, fileext = ".dcf")
  done <- FALSE
  on.exit({
    unlink(tmp)
    if (created && !done) unlink(dir, recursive = TRUE)
  }, add = TRUE)
  writeLines(enc2utf8(lines), tmp, useBytes = TRUE)

  back <- read_manifest(tmp, root, context, check = FALSE)
  differs <- if (length(back) == length(descriptors)) {
    vapply(seq_along(descriptors), function(i) !identical(back[[i]], descriptors[[i]]), logical(1))
  } else {
    rep(TRUE, length(descriptors))
  }
  if (length(back) != length(descriptors) || any(differs)) {
    pd_abort("Cannot write manifest: resource(s) ", quote_names(ids[differs]),
             " would not read back unchanged.", class = "projectData_manifest_error")
  }
  if (!file.rename(tmp, file)) pd_abort("Could not write '", file, "'.")
  done <- TRUE

  skipped <- status != "ran" & grepl("not registered", status)
  if (any(skipped)) {
    message(pd_condition(
      paste0("Loader validation skipped for ",
             paste0(ids[skipped], " (", vapply(descriptors[skipped], function(d) d$loader, ""), ")",
                    collapse = ", "),
             ": the loader is not registered, so only the structure was checked. ",
             "Register the adapter and run data_manifest_validate() to check them.\n"),
      "projectData_validation_skipped", "message"))
  }
  invisible(structure(
    data.frame(resource = ids, loader = vapply(descriptors, function(d) d$loader, ""),
               loader_validation = status, stringsAsFactors = FALSE),
    file = file
  ))
}

# Puts a descriptor in the exact form a manifest read would produce, or says why
# a manifest cannot hold it.
prepare_manifest_descriptor <- function(d) {
  if (!is.list(d) || is.null(names(d))) pd_abort("Each descriptor must be a named list.")
  bad <- function(...) {
    pd_abort("Cannot write resource ", deparse(d$resource), ": ", ...,
             class = "projectData_manifest_error")
  }
  for (f in c("title", "help_topic", "recorded_class")) {
    if (length(d[[f]]) > 1L) {
      if (length(unique(d[[f]])) > 1L) {
        bad("`", f, "` differs between objects; a manifest records one value per resource.")
      }
      d[[f]] <- d[[f]][[1]]
    }
  }
  if (is.list(d$adapter)) {
    extra <- setdiff(names(d$adapter), c("package", "export"))
    if (length(extra)) bad("adapter has unknown element(s) ", quote_names(extra))
    d$adapter <- list(package = d$adapter$package,
                      export = d$adapter$export %||% manifest_default_export)
  } else if (!is.null(d$adapter)) {
    bad("adapter must be a list with `package` and optionally `export`.")
  }
  d <- normalize_descriptor(d, default_loader = NULL, origin = "manifest")
  if (!length(d$options)) d$options <- list()

  text_ok <- function(what, x) {
    if (is.null(x) || (length(x) == 1L && is.na(x))) return(invisible())
    if (!is.character(x) || anyNA(x) || any(!nzchar(x))) bad(what, " must be non-empty text.")
    if (any(grepl("[\r\n]", x))) bad(what, " cannot contain a line break.")
    if (any(x != trimws(x))) bad(what, " cannot start or end with whitespace.")
  }
  text_ok("`resource`", d$resource)
  text_ok("`loader`", d$loader)
  text_ok("`source`", d$source)
  text_ok("`title`", unique(d$title))
  text_ok("`help_topic`", unique(d$help_topic))
  text_ok("`recorded_class`", unique(d$recorded_class))
  text_ok("object names", d$objects)
  if (any(grepl(",", d$objects, fixed = TRUE))) bad("object names cannot contain a comma.")
  if (!is.null(d$adapter)) {
    text_ok("adapter package", d$adapter$package)
    text_ok("adapter export", d$adapter$export)
  }
  if (length(d$options)) {
    keys <- names(d$options)
    if (is.null(keys) || anyDuplicated(keys)) bad("options must have unique names.")
    text_ok("option names", keys)
    if (any(grepl("[=;]", keys))) bad("option names cannot contain '=' or ';'.")
    for (k in keys) {
      v <- d$options[[k]]
      if (!is.character(v) || length(v) != 1L) bad("option '", k, "' must be a single character value.")
      text_ok(paste0("option '", k, "'"), v)
      if (grepl(";", v, fixed = TRUE)) bad("option '", k, "' cannot contain ';'.")
    }
  }
  d
}

manifest_record <- function(d) {
  opts <- if (length(d$options)) {
    paste(names(d$options), unlist(d$options, use.names = FALSE), sep = "=", collapse = ";")
  }
  export <- d$adapter$export
  fields <- c(
    Resource = d$resource, Loader = d$loader, Source = d$source,
    Objects = if (length(d$objects)) paste(d$objects, collapse = ","),
    Title = unique(d$title), HelpTopic = unique(d$help_topic), Options = opts,
    RecordedClass = unique(d$recorded_class),
    AdapterPackage = d$adapter$package,
    AdapterExport = if (!is.null(export) && !identical(export, manifest_default_export)) export
  )
  fields <- fields[!is.na(fields)]
  paste0(names(fields), ": ", fields)
}

inside_installed_package <- function(path) {
  p <- path
  while (!dir.exists(p) && !identical(dirname(p), p)) p <- dirname(p)
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(p, "Meta", "package.rds"))) return(TRUE)
    parent <- dirname(p)
    if (identical(parent, p)) return(FALSE)
    p <- parent
  }
}
