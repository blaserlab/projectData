# Layouts for manifest authoring ---------------------------------------------
#
# Adapter callbacks (`discover`, `validate`) and manifest `Source` paths are
# relative to the *installed* package root, where `data/` and the contents of
# `inst/` sit side by side. A source tree does not have that shape, so for a
# source package authoring builds a temporary view with the installed shape
# (links to `data/` and to each top-level entry of `inst/`). Building it reads
# no dataset values, executes nothing and copies no data: if a link cannot be
# made, staging fails with an error rather than copying, because a recursive
# copy of a large data collection is not lightweight. The view is removed
# afterwards; cleanup only removes the links (and the then-empty staging
# directory) created by the current operation, checks the result, and never
# touches or traverses their targets.

manifest_layouts <- c("auto", "installed", "source", "standalone")

read_source_description <- function(root) {
  f <- file.path(root, "DESCRIPTION")
  out <- c(Package = NA_character_, Version = NA_character_)
  if (!file.exists(f)) return(out)
  m <- tryCatch(read.dcf(f, fields = c("Package", "Version")), error = function(e) NULL)
  if (is.null(m) || !nrow(m)) return(out)
  m[1, ]
}

# Installed packages are recognized by any installation artifact, so a source
# tree is only ever a directory with none of them.
looks_installed <- function(root) {
  pkg <- read_source_description(root)[["Package"]]
  file.exists(file.path(root, "Meta", "package.rds")) || dir.exists(file.path(root, "Meta")) ||
    (!is.na(pkg) && file.exists(file.path(root, "R", paste0(pkg, ".rdb"))))
}

detect_layout <- function(root) {
  if (looks_installed(root)) return("installed")
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

# Creates one link; a separate function so tests can force failure.
make_link <- function(from, to) {
  isTRUE(suppressWarnings(file.symlink(from, to)))
}

# The target of a link, or NA when `path` is not a link, does not exist, or the
# platform cannot read links. (Sys.readlink() gives NA for a missing path and ""
# for a non-link, and nzchar(NA) is TRUE, so both are normalized here.)
link_target <- function(path) {
  target <- Sys.readlink(path)
  if (is.na(target) || !nzchar(target)) NA_character_ else target
}

# Whether `path` is still an entry of the file system, without following a
# link: a dangling link counts, file.exists() alone would say it is gone.
entry_exists <- function(path) {
  !is.na(link_target(path)) || file.exists(path)
}

# Removes one link as a link: unlink(recursive = FALSE) does not descend into
# what a link points to, and, unlike file.remove(), is meant for entries that
# may be directories. The outcome is checked rather than assumed, because link
# semantics differ by platform (removal of Windows directory links was not
# tested). Returns whether the entry is gone. A separate function so tests can
# force failure.
remove_link <- function(path) {
  unlink(path, recursive = FALSE, force = FALSE)
  !entry_exists(path)
}

# Removes the staging directory, which the caller has verified is empty (so the
# recursive flag, needed to remove any directory portably, deletes nothing).
remove_empty_dir <- function(path) {
  unlink(path, recursive = TRUE, force = FALSE)
  !dir.exists(path)
}

# Whether `to` is still the link this operation made to `from`. Where the
# platform can read links, anything else (a real file or directory, or a link
# to another target) is not ours and is left alone.
is_own_link <- function(to, from) {
  target <- link_target(to)
  if (!is.na(target)) return(identical(target, from))
  # No readable link target: a real entry on a platform that can read links is
  # not ours; elsewhere the record of this operation is all there is.
  !(.Platform$OS.type == "unix" && entry_exists(to))
}

# Builds the staged view of a source package. `close()` removes exactly what
# this call created: the links it recorded, then the staging directory if it is
# empty. It never deletes a link target, never removes an entry it did not
# create, checks every removal, and reports what remains as a warning (an error
# raised during cleanup could hide the error that caused it). It is safe to
# call repeatedly.
stage_source_layout <- function(src) {
  stage <- tempfile("projectData-stage-")
  if (!dir.create(stage)) pd_abort("Could not create a staging directory under ", dirname(stage), ".")
  linked <- list()
  stage_left <- TRUE
  close <- function() {
    left <- character()
    keep <- list()
    for (l in linked) {
      if (!is_own_link(l$to, l$from)) {
        left <- c(left, l$to)
        keep[[length(keep) + 1L]] <- l
      } else if (entry_exists(l$to) && !remove_link(l$to)) {
        left <- c(left, l$to)
        keep[[length(keep) + 1L]] <- l
      }
    }
    linked <<- keep
    if (stage_left && !length(keep)) {
      if (!dir.exists(stage)) {
        stage_left <<- FALSE
      } else if (length(list.files(stage, all.files = TRUE, no.. = TRUE))) {
        left <- c(left, stage)
      } else if (remove_empty_dir(stage)) {
        stage_left <<- FALSE
      } else {
        left <- c(left, stage)
      }
    } else if (stage_left) {
      left <- c(left, stage)
    }
    if (length(left)) {
      pd_warn("Could not remove temporary staging entries created for source-tree ",
              "authoring (nothing outside them was touched; the data were not copied or ",
              "deleted): ", paste(unique(left), collapse = ", "),
              ". Remove them manually.", class = "projectData_cleanup_warning")
    }
    invisible(!length(left))
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
    if (!make_link(entries[[nm]], to)) {
      pd_abort("Could not create a link for the source entry '", entries[[nm]], "' ",
               "while building the temporary installed-layout view needed to author ",
               "against a package source tree. No data was copied: projectData does not ",
               "fall back to copying, which could duplicate a large data collection. ",
               "Author from an environment that can create links, or pass an installed ",
               "package or an explicitly prepared installed-layout directory as `root` ",
               "(with `layout = \"installed\"` or `\"standalone\"`).",
               class = "projectData_staging_error")
    }
    linked[[length(linked) + 1L]] <- list(to = to, from = entries[[nm]])
  }
  allowed <- normalizePath(c(stage, data_dir, inst)[c(TRUE, dir.exists(data_dir), dir.exists(inst))],
                           winslash = "/", mustWork = TRUE)
  # Logical roots that may be symlinked out of the tree: top-level inst/ entries only.
  attr(allowed, "inst_entries") <- if (dir.exists(inst)) list.files(inst) else character()
  out <- list(root = normalizePath(stage, winslash = "/", mustWork = TRUE), close = close,
              allowed = allowed)
  ok <- TRUE
  out
}

# The root, context and path boundary used by authoring and validation.
# Callers must run `view$close()` on exit.
authoring_view <- function(root, layout = "auto") {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  layout <- resolve_layout(root, layout)
  if (identical(layout, "source")) {
    # Re-verify here: only a tree with no installation artifacts gets the
    # relaxed source-authoring path rules.
    if (looks_installed(root)) pd_abort("`root` is an installed package, not a source tree: ", root, ".")
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
