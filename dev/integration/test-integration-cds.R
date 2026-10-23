# Optional integration check against real monocle3/BPCells. These packages are
# not dependencies of projectData; the test is skipped when they are missing.
# The loader below is defined here from monocle3's documented save/load API.
# It is not a shipped adapter, so this does not validate any particular adapter
# package, only that projectData's contract works with real CDS objects.

cds_test_loader <- function() {
  projectData::data_loader(
    "test::cds",
    storage = "disk-backed",
    validate = function(descriptor, root, context) {
      file.exists(file.path(root, descriptor$source, "cds_object.rds")) || "no cds_object.rds"
    },
    open = function(descriptor, paths, context) {
      # Observed: monocle3's loader calls SummarizedExperiment generics such as
      # assay() unqualified, so monocle3 must be attached while it runs.
      # Attach it only for the duration of this call.
      withr::local_package("monocle3")
      cds <- monocle3::load_monocle_objects(directory_path = paths$source)
      stats::setNames(list(cds), descriptor$objects)
    }
  )
}

# Detaches packages attached while a test runs.
local_restore_search <- function(env = parent.frame()) {
  before <- search()
  withr::defer({
    for (p in setdiff(search(), before)) {
      try(detach(p, character.only = TRUE, unload = FALSE), silent = TRUE)
    }
  }, envir = env)
}

# Observed with monocle3 1.4.27 / BPCells 0.3.0: load_monocle_objects() copies
# the BPCells matrix directory into a new directory under tempdir(). A restored
# CDS therefore does not reference the installed files at all; it references a
# session-lifetime working copy. This test asserts that observed behaviour.
test_that("real CDS with BPCells matrix: restored object reads from a working copy after detach and installation removal", {
  skip_on_cran()
  skip_if_not_installed("monocle3")
  skip_if_not_installed("BPCells")
  local_clean_session()
  local_restore_search()
  # monocle3/BPCells create `monocle.bpcells.*.tmp` directories in the working
  # directory, in this process and in the fixture-building subprocess. Run both
  # in a throwaway directory. The tempdir is registered first so it is removed
  # after the working directory is restored.
  work <- withr::local_tempdir("cds-work-")
  withr::local_dir(work)

  m <- matrix(as.double(rpois(60, 3)), 10, 6,
              dimnames = list(paste0("g", 1:10), paste0("c", 1:6)))
  scratch <- tempfile("cds-src-")
  dir.create(scratch)
  withr::defer(unlink(scratch, recursive = TRUE))
  saved <- file.path(scratch, "cds_saved")

  # Built in a separate process: monocle3 constructors need monocle3 attached,
  # and this session's search path must stay untouched.
  saveRDS(m, file.path(scratch, "m.rds"))
  script <- file.path(scratch, "build.R")
  writeLines(c(
    "suppressMessages({library(monocle3); library(BPCells)})",
    sprintf("m <- readRDS(%s)", deparse(file.path(scratch, "m.rds"))),
    sprintf("bp <- write_matrix_dir(convert_matrix_type(as(as(m, 'dgCMatrix'), 'IterableMatrix'), 'double'), %s)",
            deparse(file.path(scratch, "bp"))),
    "cds <- new_cell_data_set(bp, cell_metadata = data.frame(row.names = colnames(m), a = 1:6),",
    "  gene_metadata = data.frame(row.names = rownames(m), gene_short_name = rownames(m)))",
    sprintf("save_monocle_objects(cds, %s, verbose = FALSE)", deparse(saved))),
    script)
  status <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                                     stdout = FALSE, stderr = FALSE))
  skip_if(!identical(status, 0L) || !file.exists(file.path(saved, "cds_object.rds")),
          "could not build a CDS fixture with the installed monocle3/BPCells")

  lib <- fixture_library()
  install_dir <- function(p) {
    dir.create(p)
    file.copy(list.files(saved, full.names = TRUE), p, recursive = TRUE)
  }
  fixture_package(lib, "pdcds", lazy = FALSE, datasets = list(),
    files = list("extdata/cds_inst" = install_dir),
    manifest = c("ManifestVersion: 1", "",
                 "Resource: cds_inst", "Loader: test::cds", "Source: extdata/cds_inst",
                 "Objects: cds_inst"))
  data_loader_add(cds_test_loader())

  installed <- list.files(file.path(lib, "pdcds", "extdata", "cds_inst"),
                          recursive = TRUE, full.names = TRUE)
  before <- unname(tools::md5sum(installed))

  cds <- data_get("cds_inst", package = "pdcds", lib.loc = lib)
  data_detach()
  expect_identical(unname(tools::md5sum(installed)), before)

  read_counts <- function(x) {
    withr::local_package("monocle3")
    as.matrix(suppressMessages(suppressWarnings(monocle3::exprs(x))))
  }
  expect_equal(unname(read_counts(cds)), unname(m))

  mat_dir <- local({ withr::local_package("monocle3"); monocle3::exprs(cds)@dir })
  expect_false(startsWith(normalizePath(mat_dir), normalizePath(lib)))

  unlink(file.path(lib, "pdcds"), recursive = TRUE)
  expect_true(dir.exists(mat_dir))
  expect_equal(unname(read_counts(cds)), unname(m))
})
