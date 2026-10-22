# Authoring against a package source tree (revision cycle 2, issue 2.1).

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

# Records what discover/validate receive; tinydisk logic does the real work, so
# the callbacks must find extdata/ and the descriptor's source below `root`.
recording_adapters <- function() {
  log <- new.env()
  log$discover <- list()
  log$validate <- list()
  log$open <- 0L
  tm <- tinydisk_loader()
  tm$id <- "srcadp::tinymat"
  d <- tm$discover
  v <- tm$validate
  o <- tm$open
  tm$discover <- function(root, context) {
    log$discover[[length(log$discover) + 1L]] <- list(root = root, context = context,
                                                       entries = list.files(root))
    d(root, context)
  }
  tm$validate <- function(descriptor, root, context) {
    log$validate[[length(log$validate) + 1L]] <- list(descriptor = descriptor, root = root,
                                                       context = context,
                                                       exists = file.exists(file.path(root, descriptor$source)))
    v(descriptor, root, context)
  }
  tm$open <- function(descriptor, paths, context) { log$open <- log$open + 1L; o(descriptor, paths, context) }

  # Claims the standard dataset `example` (and the script dataset).
  rda <- projectData::data_loader(
    "srcadp::rda",
    discover = function(root, context) {
      log$discover[[length(log$discover) + 1L]] <- list(root = root, context = context,
                                                         entries = list.files(root))
      list(list(resource = "example", source = "data/example.rda", objects = "example_obj"))
    },
    validate = function(descriptor, root, context) {
      log$validate[[length(log$validate) + 1L]] <- list(descriptor = descriptor, root = root,
                                                         context = context,
                                                         exists = file.exists(file.path(root, descriptor$source)))
      file.exists(file.path(root, descriptor$source)) || "data file missing"
    },
    open = function(descriptor, paths, context) {
      log$open <- log$open + 1L
      stats::setNames(list(paste("adapter read", basename(paths$source))), descriptor$objects)
    })
  list(tinymat = tm, rda = rda, log = log)
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

test_that("a source tree drafts adapter claims with installed-relative paths and real identity", {
  local_clean_session()
  fx <- local_source_pkg()
  ad <- recording_adapters()
  data_loader_add(ad$tinymat)
  d <- data_manifest_draft(fx$src)
  expect_equal(vapply(d, function(x) x$resource, ""), "counts")
  expect_equal(d[[1]]$source, "extdata/counts.tinymat")
  expect_equal(nrow(attr(d, "conflicts")), 0L)

  # discover saw an installed-shaped root and the source package identity
  got <- ad$log$discover[[1]]
  expect_true(all(c("data", "extdata", "projectData") %in% c(got$entries, "projectData")))
  expect_true(all(c("data", "extdata") %in% got$entries))
  expect_equal(got$context$package, "pdsrc")
  expect_equal(got$context$version, "0.4.2")
  expect_true(is.na(got$context$library))
  expect_true(is.na(got$context$collection))
  expect_true(is.na(got$context$resource))
  expect_false(identical(basename(got$context$root), "inst"))

  # validation ran against a usable path and the same identity
  v <- ad$log$validate[[1]]
  expect_true(v$exists)
  expect_equal(v$context$package, "pdsrc")
  expect_equal(v$context$resource, "counts")
  expect_equal(ad$log$open, 0L)
  # the temporary view is gone
  expect_length(stage_dirs(), 0L)
})

test_that("standard datasets in a source tree follow the existing claim rules", {
  local_clean_session()
  fx <- local_source_pkg()
  ad <- recording_adapters()
  data_loader_add(ad$tinymat)
  data_loader_add(ad$rda)

  expect_warning(d <- data_manifest_draft(fx$src), class = "projectData_draft_conflict")
  expect_equal(vapply(d, function(x) x$resource, ""), "counts")
  cf <- attr(d, "conflicts")
  expect_equal(cf$resource, "example")
  expect_match(cf$reason, "standard dataset")

  # Naming the loader makes the claim a deliberate reassignment.
  d <- data_manifest_draft(fx$src, loaders = "srcadp::rda")
  expect_equal(d[[1]]$source, "data/example.rda")
  expect_equal(d[[1]]$loader, "srcadp::rda")
  expect_false(file.exists(fx$marker))
})

test_that("a specialized-reader override for a standard resource is written and validated", {
  local_clean_session()
  fx <- local_source_pkg(corrupt_rda = TRUE)
  ad <- recording_adapters()
  data_loader_add(ad$tinymat)
  data_loader_add(ad$rda)
  before <- tree_files(fx$src)

  over <- data_manifest_draft(fx$src, loaders = "srcadp::rda")
  tiny <- data_manifest_draft(fx$src, loaders = "srcadp::tinymat")
  file <- file.path(fx$src, "inst", "projectData", "manifest.dcf")
  res <- data_manifest_write(c(over, tiny), file)
  expect_equal(sort(res$resource), c("counts", "example"))
  expect_equal(unique(res$loader_validation), "ran")

  txt <- readLines(file)
  expect_true("Source: data/example.rda" %in% txt)
  expect_true("Source: extdata/counts.tinymat" %in% txt)
  src_lines <- sub("^Source: ", "", grep("^Source:", txt, value = TRUE))
  expect_false(any(grepl("^(/|[A-Za-z]:)|inst/|projectData-stage|tmp", src_lines)))
  expect_false(any(grepl(fx$src, txt, fixed = TRUE)))

  # default root for the conventional location is the package source
  expect_no_error(back <- data_manifest_validate(file))
  expect_equal(vapply(back, function(x) x$source, "")[order(vapply(back, function(x) x$resource, ""))],
               c("extdata/counts.tinymat", "data/example.rda"))
  v <- ad$log$validate[[length(ad$log$validate)]]
  expect_equal(v$context$package, "pdsrc")
  expect_true(v$exists)

  # no data opened, no script run, source tree otherwise unchanged
  expect_equal(ad$log$open, 0L)
  expect_false(file.exists(fx$marker))
  after <- tree_files(fx$src)
  new <- startsWith(after$f, dirname(file))
  expect_equal(after$f[new], c(dirname(file), file))
  expect_equal(after[!new, ], before, ignore_attr = TRUE)
  expect_length(stage_dirs(), 0L)
})

test_that("an authored manifest installs into a usable collection", {
  local_clean_session()
  fx <- local_source_pkg()
  ad <- recording_adapters()
  data_loader_add(ad$tinymat)
  data_loader_add(ad$rda)
  d <- c(data_manifest_draft(fx$src, loaders = "srcadp::rda"),
         data_manifest_draft(fx$src, loaders = "srcadp::tinymat"))
  data_manifest_write(d, file.path(fx$src, "inst", "projectData", "manifest.dcf"))
  expect_equal(ad$log$open, 0L)

  lib <- fixture_library()
  install_source_pkg(fx$src, lib)
  # R CMD INSTALL itself evaluates data scripts to index them; only what
  # projectData does afterwards is under test.
  unlink(fx$marker)
  # a fresh session state: the authored manifest alone must drive the collection
  data_loader_remove("srcadp::rda")
  data_loader_remove("srcadp::tinymat")
  ad2 <- recording_adapters()
  data_loader_add(ad2$tinymat)
  data_loader_add(ad2$rda)
  data_attach("pdsrc", lib.loc = lib)
  expect_equal(ad2$log$open, 0L)
  expect_false(file.exists(fx$marker))

  cat <- data_catalog()
  cat <- cat[cat$collection == "pdsrc", ]
  expect_equal(cat$loader[cat$resource == "example"], "srcadp::rda")
  expect_equal(cat$origin[cat$resource == "example"], "manifest")
  expect_equal(cat$origin[cat$resource == "counts"], "manifest")
  expect_equal(cat$loader[cat$resource == "script_ds"], "projectData::dataset")
  expect_false(cat$mapping[cat$resource == "script_ds"] %in% c("verified", "mismatch"))

  expect_equal(data_get("example_obj"), "adapter read example.rda")
  m <- data_get("counts")
  expect_equal(dim(m), c(2L, 3L))
  expect_equal(m[2, 3], 6)
  expect_equal(ad2$log$open, 2L)
  # the installed callbacks saw the installed identity, not the source one
  v <- ad2$log$validate[[length(ad2$log$validate)]]
  expect_equal(v$context$package, "pdsrc")
  expect_false(is.na(v$context$library))
  expect_false(file.exists(fx$marker))
})

test_that("inst/ and bare resource directories remain standalone roots", {
  local_clean_session()
  inst <- local_inst_tree()
  ad <- recording_adapters()
  data_loader_add(ad$tinymat)
  d <- data_manifest_draft(inst)
  expect_equal(vapply(d, function(x) x$source, ""), c("extdata/alpha.tinymat", "extdata/bmat.tinymat"))
  ctx <- ad$log$discover[[1]]$context
  expect_true(is.na(ctx$package))
  expect_true(is.na(ctx$version))
  expect_true(is.na(ctx$library))
  expect_equal(ctx$root, normalizePath(inst, winslash = "/"))

  # `inst/` of a source tree, passed explicitly, is still a standalone root
  fx <- local_source_pkg()
  d <- data_manifest_draft(file.path(fx$src, "inst"))
  expect_equal(d[[1]]$source, "extdata/counts.tinymat")
  expect_true(is.na(ad$log$discover[[length(ad$log$discover)]]$context$package))
  expect_error(data_manifest_write(list(resource = "example", loader = "srcadp::rda",
                                        source = "data/example.rda"),
                                   file.path(tempfile(), "m.dcf"), root = file.path(fx$src, "inst")),
               "does not exist")
  expect_error(data_manifest_draft(inst, layout = "source"), "not a package source tree")
  expect_error(data_manifest_draft(fx$src, layout = "installed"), "not an installed package")
})

test_that("installed roots are used as installed and never modified", {
  local_clean_session()
  fx <- local_source_pkg()
  lib <- fixture_library()
  installed <- install_source_pkg(fx$src, lib)
  ad <- recording_adapters()
  data_loader_add(ad$tinymat)
  before <- tree_files(installed)
  d <- data_manifest_draft(installed)
  expect_equal(d[[1]]$source, "extdata/counts.tinymat")
  ctx <- ad$log$discover[[1]]$context
  expect_equal(ctx$package, "pdsrc")
  expect_equal(ctx$version, "0.4.2")
  expect_equal(ctx$library, normalizePath(lib, winslash = "/"))
  expect_equal(ctx$root, normalizePath(installed, winslash = "/"))

  target <- file.path(installed, "projectData", "manifest.dcf")
  expect_error(data_manifest_write(d, target, root = installed), "installed package")
  expect_error(data_manifest_write(d, target), "installed package")
  expect_equal(tree_files(installed), before)
})

test_that("unsafe sources are rejected in a source tree", {
  local_clean_session()
  fx <- local_source_pkg()
  ad <- recording_adapters()
  data_loader_add(ad$tinymat)
  data_loader_add(ad$rda)
  file <- file.path(fx$src, "inst", "projectData", "manifest.dcf")
  base <- list(resource = "example", loader = "srcadp::rda", source = "data/example.rda",
               objects = "example_obj")
  w <- function(...) suppressMessages(data_manifest_write(modifyList(base, list(...)), file))
  expect_error(w(source = "../DESCRIPTION"), "relative path")
  expect_error(w(source = "/etc/passwd"), "relative path")
  expect_error(w(source = "data/../DESCRIPTION"), "relative path")
  expect_error(w(source = "inst/extdata/counts.tinymat"), "does not exist")
  expect_error(w(source = "DESCRIPTION"), "does not exist")
  expect_false(file.exists(file))

  # a symlink inside the package that leaves it is refused
  outside <- withr::local_tempdir()
  writeLines("secret", file.path(outside, "secret.rda"))
  link <- file.path(fx$src, "inst", "extdata", "escape.rda")
  skip_if_not(isTRUE(suppressWarnings(file.symlink(file.path(outside, "secret.rda"), link))))
  expect_error(w(source = "extdata/escape.rda"), "does not exist")
  expect_false(file.exists(file))
  # links to the staged view did not damage the source tree
  expect_true(file.exists(file.path(fx$src, "data", "example.rda")))
  expect_length(stage_dirs(), 0L)
})

test_that("existing manifests are not overwritten and failed writes leave nothing", {
  local_clean_session()
  fx <- local_source_pkg()
  data_loader_add(recording_adapters()$tinymat)
  file <- file.path(fx$src, "inst", "projectData", "manifest.dcf")
  d <- data_manifest_draft(fx$src)
  data_manifest_write(d, file)
  before <- readLines(file)
  expect_error(data_manifest_write(d, file), "already exists")
  bad <- modifyList(d[[1]], list(source = "extdata/none.tinymat"))
  expect_error(data_manifest_write(list(bad), file, overwrite = TRUE), "does not exist")
  expect_identical(readLines(file), before)
  expect_equal(list.files(dirname(file), all.files = TRUE, no.. = TRUE), "manifest.dcf")
  expect_length(stage_dirs(), 0L)
})
