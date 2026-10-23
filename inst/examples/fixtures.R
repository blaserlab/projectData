# Builds small installed fixture packages in a temporary library.
# Used by the tests and the vignette. Not part of the projectData API.

fixture_library <- function() {
  lib <- tempfile("pd-lib-")
  dir.create(lib)
  lib
}

# datasets: named list, file stem -> named list of objects saved in <stem>.rda
# documented: named list, stem -> object names given an \alias (default all)
# files: named list, relative path in the package root (under inst/) -> function(path)
# scripts: named list, stem -> character vector of lines written to
#   data/<stem>.R (script datasets; use lazy = FALSE to keep them runtime-evaluated).
#   `script_docs` gives the object names documented for each script stem.
# data_files: named list, file name under data/ -> function(path), for other
#   formats (text, csv, a second file sharing a stem with a script, ...).
# code: character vector of R source lines (installed as R/code.R); `exports`
#   names the functions to export (so the package can be attached for tests).
# drop_index: stems whose rows are removed from Meta/data.rds after install,
#   simulating an incomplete index.
fixture_package <- function(lib, name, version = "0.1.0", lazy = TRUE,
                            datasets = list(), documented = NULL,
                            files = list(), manifest = NULL,
                            drop_index = character(),
                            scripts = list(), script_docs = NULL,
                            code = NULL, exports = character(), data_files = list()) {
  src <- file.path(tempfile("pd-src-"), name)
  on.exit(unlink(dirname(src), recursive = TRUE), add = TRUE)
  dir.create(file.path(src, "man"), recursive = TRUE)
  writeLines(c(paste("Package:", name), "Title: Fixture", paste("Version:", version),
               "Description: Synthetic fixture.", "License: GPL-3", "Author: fixture",
               paste("LazyData:", if (lazy) "true" else "false")),
             file.path(src, "DESCRIPTION"))
  writeLines(paste0("export(", exports, ")"), file.path(src, "NAMESPACE"))
  if (length(code)) {
    dir.create(file.path(src, "R"))
    writeLines(code, file.path(src, "R", "code.R"))
  }
  if (length(datasets) || length(scripts) || length(data_files)) dir.create(file.path(src, "data"))
  for (stem in names(scripts)) {
    writeLines(scripts[[stem]], file.path(src, "data", paste0(stem, ".R")))
    docs <- if (is.null(script_docs)) stem else script_docs[[stem]]
    writeLines(c(paste0("\\name{", stem, "}"), "\\docType{data}",
                 paste0("\\alias{", unique(c(stem, docs)), "}"),
                 paste0("\\title{Title of ", stem, "}"), "\\description{Fixture.}",
                 paste0("\\usage{", docs[1], "}"), "\\format{Fixture.}"),
               file.path(src, "man", paste0(stem, ".Rd")))
  }
  for (stem in names(datasets)) {
    objs <- datasets[[stem]]
    save(list = names(objs), file = file.path(src, "data", paste0(stem, ".rda")),
         envir = list2env(objs))
    docs <- if (is.null(documented)) names(objs) else documented[[stem]]
    if (length(docs)) {
      writeLines(c(paste0("\\name{", stem, "}"), "\\docType{data}",
                   paste0("\\alias{", unique(c(stem, docs)), "}"),
                   paste0("\\title{Title of ", stem, "}"), "\\description{Fixture.}",
                   paste0("\\usage{", docs[1], "}"), "\\format{Fixture.}"),
                 file.path(src, "man", paste0(stem, ".Rd")))
    }
  }
  for (f in names(data_files)) data_files[[f]](file.path(src, "data", f))
  for (rel in names(files)) {
    path <- file.path(src, "inst", rel)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    files[[rel]](path)
  }
  if (!is.null(manifest)) {
    dir.create(file.path(src, "inst", "projectData"), recursive = TRUE, showWarnings = FALSE)
    writeLines(manifest, file.path(src, "inst", "projectData", "manifest.dcf"))
  }
  out <- system2(file.path(R.home("bin"), "R"),
                 c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(src)),
                 stdout = TRUE, stderr = TRUE)
  if (!file.exists(file.path(lib, name, "DESCRIPTION"))) {
    stop("Fixture install failed:\n", paste(out, collapse = "\n"))
  }
  for (stem in drop_index) {
    f <- file.path(lib, name, "Meta", "data.rds")
    idx <- readRDS(f)
    keep <- !grepl(paste0("(^|\\()", stem, "\\)?$"), idx[, 1])
    saveRDS(idx[keep, , drop = FALSE], f)
  }
  invisible(file.path(lib, name))
}
