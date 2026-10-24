# A source package: DESCRIPTION, a standard .rda dataset, a script dataset and
# an adapter-managed tinymat under inst/extdata. The script writes a marker if
# it is ever evaluated; `corrupt_rda` makes any attempt to read the .rda fail.
local_source_pkg <- function(name = "pdsrc", version = "0.4.2", corrupt_rda = FALSE,
                             env = parent.frame()) {
  top <- withr::local_tempdir(.local_envir = env)
  src <- file.path(top, name)
  dir.create(file.path(src, "data"), recursive = TRUE)
  marker <- file.path(top, "script-ran")
  writeLines(c(paste("Package:", name), "Title: Fixture", paste("Version:", version),
               "Description: Synthetic fixture.", "License: GPL-3", "Author: fixture",
               "LazyData: false"), file.path(src, "DESCRIPTION"))
  writeLines("", file.path(src, "NAMESPACE"))
  if (corrupt_rda) {
    writeLines("not an rda file", file.path(src, "data", "example.rda"))
  } else {
    example_obj <- "rda-value"
    save(example_obj, file = file.path(src, "data", "example.rda"))
  }
  writeLines(c(sprintf("writeLines('ran', %s)", deparse(marker)), "script_obj <- 1"),
             file.path(src, "data", "script_ds.R"))
  tinymat_write(matrix(as.double(1:6), 2, 3), file.path(src, "inst", "extdata", "counts.tinymat"))
  list(src = src, marker = marker)
}

install_source_pkg <- function(src, lib) {
  out <- system2(file.path(R.home("bin"), "R"),
                 c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(src)),
                 stdout = TRUE, stderr = TRUE)
  if (!file.exists(file.path(lib, basename(src), "DESCRIPTION"))) stop(paste(out, collapse = "\n"))
  invisible(file.path(lib, basename(src)))
}

tree_files <- function(path) {
  f <- list.files(path, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, full.names = TRUE)
  f <- f[order(f)]
  sums <- ifelse(dir.exists(f), NA_character_, unname(tools::md5sum(f[!dir.exists(f)])[f]))
  data.frame(f = f, size = file.size(f), sum = unname(sums))
}

stage_dirs <- function() list.files(tempdir(), "^projectData-stage-")
