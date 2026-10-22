manifest_path <- "projectData/manifest.dcf"
manifest_fields <- c("ManifestVersion", "Resource", "Loader", "Source", "Objects",
                     "Title", "HelpTopic", "Options", "AdapterPackage",
                     "AdapterExport", "RecordedClass")

# Installation ---------------------------------------------------------------

resolve_installation <- function(package, lib.loc) {
  if (!is_scalar_string(package)) pd_abort("`package` must be a package name.")
  search_libs <- lib.loc %||% .libPaths()
  root <- find.package(package, lib.loc = search_libs, quiet = TRUE)
  root <- root[file.exists(file.path(root, "Meta", "package.rds"))]
  if (!length(root)) {
    pd_abort("Package '", package, "' is not installed in: ",
             paste(search_libs, collapse = ", "), ".",
             class = "projectData_not_installed")
  }
  root <- normalizePath(root[[1]], winslash = "/", mustWork = TRUE)
  desc <- installed_description(root)
  list(root = root, library = dirname(root), version = desc[["Version"]],
       identity = installed_identity(root))
}

installed_description <- function(root) {
  readRDS(file.path(root, "Meta", "package.rds"))$DESCRIPTION
}

# Version alone is insufficient: Built has one-second resolution, so the
# installed index's size and modification time are added. The (small) metadata
# file is stat()ed, never the data.
installed_identity <- function(root) {
  desc <- installed_description(root)
  info <- file.info(file.path(root, "Meta", "package.rds"))
  paste(desc[["Version"]], desc[["Built"]] %||% NA_character_,
        format(as.numeric(info$mtime), digits = 17), info$size, sep = " | ")
}

# Standard datasets ----------------------------------------------------------

data_file_pattern <- paste0(
  "\\.(R|r|RData|rdata|rda|tab|txt|TXT|csv|CSV)(\\.(gz|bz2|xz))?$"
)

# Index items look like "obj" or "obj (dataset)"; keep the raw item.
parse_index_items <- function(items) {
  m <- regexec("^(.+?) \\((.+)\\)$", items)
  parts <- regmatches(items, m)
  has_ds <- lengths(parts) == 3L
  data.frame(
    item = items,
    object = ifelse(has_ds, vapply(parts, function(p) p[2], ""), items),
    dataset = ifelse(has_ds, vapply(parts, function(p) p[3], ""), items),
    stringsAsFactors = FALSE
  )
}

inventory_datasets <- function(root) {
  idx_file <- file.path(root, "Meta", "data.rds")
  data_dir <- file.path(root, "data")
  if (!file.exists(idx_file) && !dir.exists(data_dir)) return(list())

  index <- if (file.exists(idx_file)) readRDS(idx_file) else matrix(character(), 0, 2)
  parsed <- parse_index_items(as.character(index[, 1]))
  parsed$title <- as.character(index[, 2])
  parsed$title[!nzchar(parsed$title)] <- NA_character_

  lazy_file <- file.path(data_dir, "Rdata.rds")
  datasets <- list()
  if (file.exists(lazy_file)) {
    # Lazy-load maps dataset -> object names; only this small map is read.
    map <- readRDS(lazy_file)
    for (ds in names(map)) {
      datasets[[ds]] <- list(source = "data/Rdata.rdb", objects = as.character(map[[ds]]))
    }
  } else if (dir.exists(data_dir)) {
    # `datalist` is written by R CMD build/INSTALL as an index of the data
    # directory; it is not a dataset.
    files <- setdiff(list.files(data_dir), "datalist")
    ok <- grepl(data_file_pattern, files)
    if (any(!ok)) {
      pd_warn("Unsupported files in data/ were not inventoried: ",
              quote_names(files[!ok]), ". Initial support covers the formats ",
              "understood by utils::data().", class = "projectData_unsupported_format")
    }
    for (f in files[ok]) {
      ds <- sub(data_file_pattern, "", f)
      datasets[[ds]] <- list(source = paste0("data/", f), objects = NULL)
    }
  }
  # Indexed datasets without a located file are kept; opening will report.
  for (ds in setdiff(unique(parsed$dataset), names(datasets))) {
    datasets[[ds]] <- list(source = NA_character_, objects = NULL)
  }

  lapply(names(datasets), function(ds) {
    d <- datasets[[ds]]
    rows <- parsed[parsed$dataset == ds, , drop = FALSE]
    objs <- d$objects %||% (if (nrow(rows)) rows$object else NULL)
    topic <- if (is.null(objs)) NULL else {
      ti <- rows$title[match(objs, rows$object)]
      ifelse(is.na(ti), NA_character_, objs)
    }
    list(resource = ds, loader = "projectData::dataset", source = d$source,
         objects = objs, options = list(),
         title = if (is.null(objs)) NA_character_ else rows$title[match(objs, rows$object)],
         help_topic = topic, recorded_class = NA_character_, adapter = NULL,
         origin = "index")
  })
}

# Manifest -------------------------------------------------------------------

#' Validate a projectData manifest
#'
#' A manifest is an optional, versioned DCF file at
#' `projectData/manifest.dcf` in the installed package (`inst/projectData/`
#' in the source). It supplies mappings or loader declarations that cannot be
#' discovered automatically. It contains data only: relative paths and loader
#' references, never code.
#'
#' The first record holds `ManifestVersion: 1`. Each following record
#' describes one resource with fields `Resource` (required), `Loader`
#' (required, namespaced id), `Source` (path relative to the package root),
#' `Objects` (comma-separated), `Title`, `HelpTopic`, `Options`
#' (`key=value` pairs separated by `;`), `RecordedClass`, and optionally
#' `AdapterPackage` and `AdapterExport` naming a package export (a function
#' returning a [data_loader()]) that is loaded by namespace on first access if
#' the loader is not yet registered.
#'
#' A manifest record replaces every automatically discovered candidate with
#' the same `Resource`. Declaring a resource twice in one manifest, whether or
#' not the records agree, is an error.
#'
#' @param file Path to a manifest file.
#' @param root Package root against which `Source` paths are checked. Defaults
#'   to two directories above `file`.
#' @return A list of validated descriptors, invisibly.
#' @seealso [data_manifest_draft()] and [data_manifest_write()] to produce a
#'   manifest.
#' @export
data_manifest_validate <- function(file, root = dirname(dirname(file))) {
  invisible(read_manifest(file, root, root_context(root)))
}

# `check = FALSE` only parses; used to verify what the writer produced.
read_manifest <- function(file, root, context, check = TRUE) {
  fail <- function(...) pd_abort("Invalid manifest ", basename(dirname(file)), "/",
                                 basename(file), ": ", ..., class = "projectData_manifest_error")
  m <- tryCatch(read.dcf(file), error = function(e) fail("not readable as DCF (", conditionMessage(e), ")"))
  unknown <- setdiff(colnames(m), manifest_fields)
  if (length(unknown)) fail("unknown field(s) ", quote_names(unknown))
  get <- function(i, f) if (f %in% colnames(m)) unname(m[i, f]) else NA_character_
  if (!nrow(m) || is.na(get(1, "ManifestVersion"))) {
    fail("the first record must contain ManifestVersion")
  }
  if (!identical(get(1, "ManifestVersion"), "1")) {
    fail("unsupported ManifestVersion '", get(1, "ManifestVersion"), "' (supported: 1)")
  }
  rows <- seq_len(nrow(m))[-1]
  ids <- vapply(rows, function(i) get(i, "Resource"), "")
  if (anyNA(ids)) fail("every resource record needs a Resource field")
  if (anyDuplicated(ids)) {
    dup <- unique(ids[duplicated(ids)])
    loaders <- vapply(dup, function(r) {
      paste(vapply(rows[ids == r], function(i) get(i, "Loader") %||% "NA", ""), collapse = " vs ")
    }, "")
    fail("duplicate Resource ", quote_names(dup), " (loaders: ", paste(loaders, collapse = "; "),
         "); declare each resource once")
  }

  lapply(rows, function(i) {
    split_csv <- function(x) if (is.na(x)) NULL else trimws(strsplit(x, ",", fixed = TRUE)[[1]])
    loader <- get(i, "Loader")
    if (is.na(loader)) fail("resource '", get(i, "Resource"), "' needs a Loader")
    opts <- list()
    if (!is.na(get(i, "Options"))) {
      kv <- strsplit(strsplit(get(i, "Options"), ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
      if (any(lengths(kv) < 2L)) fail("Options must be key=value pairs separated by ';'")
      opts <- stats::setNames(lapply(kv, function(p) trimws(paste(p[-1], collapse = "="))),
                              trimws(vapply(kv, `[`, "", 1)))
    }
    adapter <- NULL
    if (!is.na(get(i, "AdapterPackage"))) {
      adapter <- list(package = get(i, "AdapterPackage"),
                      export = if (is.na(get(i, "AdapterExport"))) "projectData_loaders" else get(i, "AdapterExport"))
    }
    objs <- split_csv(get(i, "Objects"))
    d <- list(resource = get(i, "Resource"), loader = loader,
              source = get(i, "Source"), objects = objs, options = opts,
              title = get(i, "Title"), help_topic = get(i, "HelpTopic"),
              recorded_class = get(i, "RecordedClass"), adapter = adapter,
              origin = "manifest")
    d <- normalize_descriptor(d, default_loader = NULL)
    if (check) check_descriptor(d, root, context, fail = fail)
    d
  })
}

# Descriptors ----------------------------------------------------------------

normalize_descriptor <- function(d, default_loader, origin = d$origin) {
  if (!is.list(d)) pd_abort("A descriptor must be a list.")
  unknown <- setdiff(names(d), c(descriptor_fields, "origin"))
  if (length(unknown)) {
    pd_abort("Descriptor has unknown field(s) ", quote_names(unknown),
             ". Allowed: ", paste(descriptor_fields, collapse = ", "), ".")
  }
  d$loader <- d$loader %||% default_loader
  d$source <- d$source %||% NA_character_
  d$options <- d$options %||% list()
  n <- max(1L, length(d$objects))
  rec <- function(x) {
    x <- x %||% NA_character_
    if (length(x) == 1L) rep(x, n) else x
  }
  d$title <- rec(d$title)
  d$help_topic <- rec(d$help_topic)
  d$recorded_class <- rec(d$recorded_class)
  for (f in c("title", "help_topic", "recorded_class")) {
    if (length(d[[f]]) != n) pd_abort("Descriptor field `", f, "` must have length 1 or one value per object.")
  }
  list(resource = d$resource, loader = d$loader, source = d$source,
       objects = d$objects, options = d$options, title = d$title,
       help_topic = d$help_topic, recorded_class = d$recorded_class,
       adapter = d$adapter, origin = origin %||% "unknown")
}

check_descriptor <- function(d, root, context, fail = function(...) pd_abort(...)) {
  where <- paste0("resource ", deparse(d$resource), ": ")
  if (!is_scalar_string(d$resource)) fail("descriptor needs a non-empty `resource` id")
  if (!is_scalar_string(d$loader) || !grepl(loader_id_pattern, d$loader)) {
    fail(where, "loader must be a namespaced id like 'pkg::name'")
  }
  if (!is.na(d$source)) {
    if (!is_safe_relative_path(d$source)) {
      fail(where, "Source must be a relative path without '..' components or absolute/backslash forms; got ", deparse(d$source))
    }
    if (is.na(resolve_inside_root(root, d$source))) {
      fail(where, "Source '", d$source, "' does not exist inside the package root")
    }
  }
  if (!is.null(d$objects)) {
    if (!is.character(d$objects) || anyNA(d$objects) || any(!nzchar(d$objects)) ||
        anyDuplicated(d$objects)) {
      fail(where, "objects must be unique, non-empty names")
    }
  }
  if (!is.list(d$options) || (length(d$options) && is.null(names(d$options)))) {
    fail(where, "options must be a named list")
  }
  if (!is.null(d$adapter) && !(is_scalar_string(d$adapter$package) && is_scalar_string(d$adapter$export))) {
    fail(where, "adapter needs a package and export")
  }
  loader <- find_loader(d$loader)
  if (!is.null(loader) && !is.null(loader$validate)) {
    msg <- validate_with_loader(loader, d, root, replace_resource(context, d$resource))
    if (!is.null(msg)) fail(where, msg)
  }
  invisible(d)
}

replace_resource <- function(context, resource) {
  context$resource <- resource
  context
}

# Assembling a collection's resources ----------------------------------------

report_ignored_claims <- function(resources, loaders) {
  mode <- .state$config$ignored_claims %||% "warn"
  if (identical(mode, "silent")) return(invisible())
  txt <- paste0("Ignored adapter claim(s) on standard data resource(s); the built-in ",
                "reader keeps them: ", paste0(resources, " (", loaders, ")", collapse = ", "),
                ". Add a manifest entry to assign one to a specialized reader.")
  if (identical(mode, "message")) {
    message(pd_condition(paste0(txt, "\n"), "projectData_ignored_claim", "message"))
  } else {
    pd_warn(txt, class = "projectData_ignored_claim")
  }
  invisible()
}

# Precedence, applied to metadata only (no reader is invoked):
# 1. A manifest record for a resource replaces every other candidate for it.
# 2. Otherwise, resources in the standard data inventory belong to the built-in
#    reader; adapter claims on them are ignored with a warning.
# 3. Otherwise, several adapters claiming one resource is ambiguous and fails.
# Distinct resources that expose the same object name are a separate check,
# made last.
assemble_descriptors <- function(root, context) {
  standard <- lapply(inventory_datasets(root), normalize_descriptor,
                     default_loader = "projectData::dataset", origin = "index")
  claims <- list()
  # Sorted ids keep the outcome independent of registration order.
  for (id in sort(names(.state$loaders))) {
    l <- .state$loaders[[id]]
    if (is.null(l$discover)) next
    ds <- l$discover(root, context)
    if (is.null(ds)) next
    if (!is.list(ds)) pd_abort("Discovery by loader '", id, "' must return a list of descriptors.")
    for (d in ds) {
      claims[[length(claims) + 1L]] <-
        normalize_descriptor(d, default_loader = id, origin = paste0("adapter:", id))
    }
  }

  mf <- file.path(root, manifest_path)
  declared <- if (file.exists(mf)) read_manifest(mf, root, context) else list()
  declared_ids <- vapply(declared, function(d) d$resource, "")

  standard_ids <- vapply(standard, function(d) d$resource, "")
  claim_ids <- vapply(claims, function(d) d$resource, "")
  open_claims <- !claim_ids %in% declared_ids

  ignored <- open_claims & claim_ids %in% standard_ids
  if (any(ignored)) {
    report_ignored_claims(claim_ids[ignored],
                          sub("^adapter:", "", vapply(claims[ignored], function(d) d$origin, "")))
  }
  claims <- claims[open_claims & !ignored]
  claim_ids <- claim_ids[open_claims & !ignored]

  dup <- unique(claim_ids[duplicated(claim_ids)])
  if (length(dup)) {
    origins <- vapply(claims[claim_ids %in% dup],
                      function(d) paste0(d$resource, " (", d$origin, ")"), "")
    pd_abort("Ambiguous discovery: resource id(s) found more than once: ",
             paste(origins, collapse = ", "),
             ". Add a manifest entry to choose one explicitly.",
             class = "projectData_ambiguous_discovery")
  }

  for (d in claims) check_descriptor(d, root, context)
  candidates <- c(standard[!standard_ids %in% declared_ids], claims)
  names(candidates) <- vapply(candidates, function(d) d$resource, "")
  # A manifest entry takes the place of the candidate it replaces.
  for (d in declared) candidates[[d$resource]] <- d
  out <- unname(candidates)

  seen <- list()
  for (d in out) for (o in d$objects) {
    if (!is.null(seen[[o]])) {
      pd_abort("Object '", o, "' is produced by both resource '", seen[[o]],
               "' and '", d$resource, "' in the same collection.",
               class = "projectData_duplicate_object")
    }
    seen[[o]] <- d$resource
  }
  out
}
