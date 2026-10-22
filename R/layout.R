# Layouts for manifest authoring ---------------------------------------------
#
# Adapter callbacks (`discover`, `validate`) and manifest `Source` paths are
# relative to the *installed* package root, where `data/` and the contents of
# `inst/` sit side by side. A source tree does not have that shape, so for a
# source package authoring builds a temporary view with the installed shape
# (links to `data/` and to each top-level entry of `inst/`; a copy only where
# links cannot be made). Nothing is read or executed to build it, and the view
# is removed afterwards.

manifest_layouts <- c("auto", "installed", "source", "standalone")

read_source_description <- function(root) {
  f <- file.path(root, "DESCRIPTION")
  out <- c(Package = NA_character_, Version = NA_character_)
  if (!file.exists(f)) return(out)
  m <- tryCatch(read.dcf(f, fields = c("Package", "Version")), error = function(e) NULL)
  if (is.null(m) || !nrow(m)) return(out)
  m[1, ]
}

detect_layout <- function(root) {
  if (file.exists(file.path(root, "Meta", "package.rds"))) return("installed")
  if (!is.na(read_source_description(root)[["Package"]])) return("source")
  "standalone"
}

resolve_layout <- function(root, layout) {
  layout <- match.arg(layout, manifest_layouts)
  found <- detect_layout(root)
  if (identical(layout, "auto")) return(found)
  if (identical(layout, "installed") && !identical(found, "installed")) {
    pd_abort("`root` is not an installed package (no Meta/package.rds): ", root, ".")
  }
  if (identical(layout, "source") && identical(found, "standalone")) {
    pd_abort("`root` is not a package source tree (no DESCRIPTION with a Package field): ", root, ".")
  }
  if (identical(layout, "source") && identical(found, "installed")) {
    pd_abort("`root` is an installed package, not a source tree: ", root, ".")
  }
  layout
}

# `root` here is the directory callbacks will receive.
root_context <- function(root, layout = "standalone", source_root = root) {
  switch(layout,
    installed = {
      desc <- installed_description(root)
      loader_context(NA_character_, desc[["Package"]], desc[["Version"]], dirname(root), root)
    },
    source = {
      desc <- read_source_description(source_root)
      loader_context(NA_character_, desc[["Package"]], desc[["Version"]], NA_character_, root)
    },
    loader_context(NA_character_, NA_character_, NA_character_, NA_character_, root)
  )
}

# Builds the staged view of a source package.
stage_source_layout <- function(src) {
  stage <- tempfile("projectData-stage-")
  dir.create(stage)
  linked <- character()
  close <- function() {
    for (p in linked) unlink(p)
    unlink(stage, recursive = TRUE)
  }
  ok <- FALSE
  on.exit(if (!ok) close())

  entries <- character()
  data_dir <- file.path(src, "data")
  if (dir.exists(data_dir)) entries <- c(data = data_dir)
  inst <- file.path(src, "inst")
  if (dir.exists(inst)) {
    inst_entries <- list.files(inst)
    if ("data" %in% inst_entries && "data" %in% names(entries)) {
      pd_abort("The source tree has both data/ and inst/data; they would merge on ",
               "installation and cannot be authored against reliably.")
    }
    entries <- c(entries, stats::setNames(file.path(inst, inst_entries), inst_entries))
  }
  for (nm in names(entries)) {
    to <- file.path(stage, nm)
    if (isTRUE(suppressWarnings(file.symlink(entries[[nm]], to)))) {
      linked <- c(linked, to)
    } else if (!file.copy(entries[[nm]], stage, recursive = TRUE)) {
      pd_abort("Could not stage '", entries[[nm]], "' for authoring.")
    }
  }
  ok <- TRUE
  list(root = normalizePath(stage, winslash = "/", mustWork = TRUE), close = close,
       allowed = normalizePath(c(stage, data_dir, inst)[c(TRUE, dir.exists(data_dir), dir.exists(inst))],
                               winslash = "/", mustWork = TRUE))
}

# The root, context and path boundary used by authoring and validation.
# Callers must run `view$close()` on exit.
authoring_view <- function(root, layout = "auto") {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  layout <- resolve_layout(root, layout)
  if (identical(layout, "source")) {
    st <- stage_source_layout(root)
    return(list(layout = layout, root = st$root, source_root = root, allowed = st$allowed,
                close = st$close, context = root_context(st$root, layout, root)))
  }
  list(layout = layout, root = root, source_root = root, allowed = NULL, close = function() NULL,
       context = root_context(root, layout))
}

# For `<package source>/inst/projectData/manifest.dcf` the package source is the
# root; otherwise two directories above the file.
default_manifest_root <- function(file) {
  top <- normalizePath(dirname(dirname(file)), winslash = "/", mustWork = FALSE)
  pkg <- dirname(top)
  if (identical(basename(top), "inst") && identical(detect_layout(pkg), "source")) pkg else top
}
