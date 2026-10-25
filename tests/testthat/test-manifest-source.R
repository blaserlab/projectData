# Authoring against a package source tree (revision cycle 2, issue 2.1).


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


test_that("a source tree drafts adapter claims with installed-relative paths and real identity", {
  skip_if_no_links()
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
  skip_if_no_links()
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
  skip_if_no_links()
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
  skip_if_no_links()
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
  expect_true(projectData:::same_path(ctx$root, inst))

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
  expect_true(projectData:::same_path(ctx$library, lib))
  expect_true(projectData:::same_path(ctx$root, installed))

  target <- file.path(installed, "projectData", "manifest.dcf")
  expect_error(data_manifest_write(d, target, root = installed), "installed package")
  expect_error(data_manifest_write(d, target), "installed package")
  expect_equal(tree_files(installed), before)
})

test_that("unsafe sources are rejected in a source tree", {
  skip_if_no_links()
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

  # a data/ symlink that leaves the package is refused (only inst/ links may
  # lead outside a source tree; see test-manifest-symlinks.R)
  outside <- withr::local_tempdir()
  writeLines("secret", file.path(outside, "secret.rda"))
  link <- file.path(fx$src, "data", "escape.rda")
  expect_true(local_link(file.path(outside, "secret.rda"), link))
  expect_error(w(source = "data/escape.rda"), "does not exist")
  expect_false(file.exists(file))
  # links to the staged view did not damage the source tree
  expect_true(file.exists(file.path(fx$src, "data", "example.rda")))
  expect_length(stage_dirs(), 0L)
})

test_that("existing manifests are not overwritten and failed writes leave nothing", {
  skip_if_no_links()
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

# Link failure during staging (revision cycle 3, issue 3.1) -------------------

# Fails link creation from the `n`th call on, and records link and copy calls.
local_link_failure <- function(fail_from = 1L, env = parent.frame()) {
  rec <- new.env()
  rec$links <- character()
  rec$copies <- 0L
  testthat::local_mocked_bindings(
    make_link = function(from, to) {
      rec$links <- c(rec$links, from)
      if (length(rec$links) >= fail_from) return(FALSE)
      isTRUE(file.symlink(from, to))
    },
    .env = env
  )
  testthat::local_mocked_bindings(
    file.copy = function(...) { rec$copies <- rec$copies + 1L; FALSE },
    .package = "base", .env = env
  )
  rec
}

test_that("link failure gives a targeted error and never copies", {
  local_clean_session()
  fx <- local_source_pkg()
  rec <- local_link_failure()
  err <- expect_error(data_manifest_draft(fx$src), class = "projectData_staging_error")
  expect_match(conditionMessage(err), "No data was copied", fixed = TRUE)
  expect_mentions_path(conditionMessage(err), file.path(fx$src, "data"))
  expect_match(conditionMessage(err), "installed-layout", fixed = TRUE)
  expect_equal(rec$copies, 0L)
  expect_length(stage_dirs(), 0L)
})

test_that("failure after successful links removes the partial view and leaves the source intact", {
  skip_if_no_links()
  local_clean_session()
  fx <- local_source_pkg()
  before <- tree_files(fx$src)
  rec <- local_link_failure(fail_from = 2L)
  expect_error(data_manifest_draft(fx$src), class = "projectData_staging_error")
  expect_length(rec$links, 2L)
  expect_equal(rec$copies, 0L)
  expect_length(stage_dirs(), 0L)
  expect_equal(tree_files(fx$src), before)
})

test_that("cleanup does not follow or delete nested links that leave the source tree", {
  skip_if_no_links()
  local_clean_session()
  fx <- local_source_pkg()
  data_loader_add(recording_adapters()$tinymat)
  outside <- withr::local_tempdir()
  writeLines("keep", file.path(outside, "precious.txt"))
  expect_true(local_link(outside, file.path(fx$src, "inst", "extdata", "escape")))
  expect_true(local_link(outside, file.path(fx$src, "data", "escape")))
  dir.create(file.path(fx$src, "inst", "extra"))
  rec <- local_link_failure(fail_from = 3L)
  expect_error(data_manifest_draft(fx$src), class = "projectData_staging_error")
  expect_true(file.exists(file.path(outside, "precious.txt")))
  expect_identical(readLines(file.path(outside, "precious.txt")), "keep")
  expect_true(file.exists(file.path(fx$src, "data", "escape")))
  expect_true(file.exists(file.path(fx$src, "inst", "extdata", "escape")))
  expect_length(stage_dirs(), 0L)
})

test_that("existing manifests are unchanged when staging fails", {
  skip_if_no_links()
  local_clean_session()
  fx <- local_source_pkg()
  data_loader_add(recording_adapters()$tinymat)
  d <- data_manifest_draft(fx$src)
  mf <- file.path(fx$src, "inst", "projectData", "manifest.dcf")
  data_manifest_write(d, mf, root = fx$src)
  before <- readLines(mf)
  info <- file.info(mf)
  rec <- local_link_failure()
  expect_error(data_manifest_draft(fx$src), class = "projectData_staging_error")
  expect_error(data_manifest_validate(mf, root = fx$src), class = "projectData_staging_error")
  expect_error(data_manifest_write(d, mf, root = fx$src, overwrite = TRUE),
               class = "projectData_staging_error")
  expect_identical(readLines(mf), before)
  expect_equal(file.info(mf)$mtime, info$mtime)
  expect_equal(rec$copies, 0L)
  expect_length(list.files(dirname(mf), all.files = TRUE, no.. = TRUE), 1L)
})

test_that("successful staging still handles standard datasets and extdata resources", {
  skip_if_no_links()
  local_clean_session()
  fx <- local_source_pkg()
  ad <- recording_adapters()
  data_loader_add(ad$tinymat)
  data_loader_add(ad$rda)
  expect_equal(data_manifest_draft(fx$src, loaders = "srcadp::rda")[[1]]$source,
               "data/example.rda")
  expect_warning(d <- data_manifest_draft(fx$src), class = "projectData_draft_conflict")
  expect_equal(d[[1]]$source, "extdata/counts.tinymat")
  expect_true(all(c("data", "extdata") %in% ad$log$discover[[1]]$entries))
  expect_length(stage_dirs(), 0L)
})

test_that("installed and standalone roots work when source staging is unavailable", {
  local_clean_session()
  fx <- local_source_pkg()
  data_loader_add(recording_adapters()$tinymat)
  lib <- withr::local_tempdir()
  installed <- install_source_pkg(fx$src, lib)
  rec <- local_link_failure()
  d_inst <- data_manifest_draft(installed)
  expect_equal(vapply(d_inst, function(x) x$resource, ""), "counts")
  d_alone <- data_manifest_draft(file.path(fx$src, "inst"))
  expect_equal(vapply(d_alone, function(x) x$source, ""), "extdata/counts.tinymat")
  mf <- file.path(withr::local_tempdir(), "manifest.dcf")
  data_manifest_write(d_alone, mf, root = file.path(fx$src, "inst"))
  expect_error(data_manifest_validate(mf, root = file.path(fx$src, "inst")), NA)
  expect_length(rec$links, 0L)
  expect_equal(rec$copies, 0L)
})

# Cleanup correctness (separate from link availability) ------------------------

# Runs `code`, returning the error it raised and the warnings it signalled.
capture_error_and_warnings <- function(code) {
  warnings <- list()
  err <- withCallingHandlers(
    tryCatch({ code; NULL }, error = function(e) e),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
  list(error = err, warnings = warnings)
}

# Removes whatever a deliberately failed cleanup left behind: links as links,
# then the directory. Registered before the failure is provoked.
local_remove_stage_leftovers <- function(env = parent.frame()) {
  before <- stage_dirs()
  withr::defer({
    for (d in setdiff(stage_dirs(), before)) {
      dir <- file.path(tempdir(), d)
      for (e in list.files(dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)) {
        projectData:::remove_link(e)
      }
      unlink(dir, recursive = TRUE)
    }
  }, envir = env)
}

test_that("complete staging cleanup removes only its links and directory", {
  skip_if_no_links()
  fx <- local_source_pkg()
  outside <- withr::local_tempdir()
  writeLines("keep", file.path(outside, "precious.txt"))
  expect_true(local_link(outside, file.path(fx$src, "inst", "extdata", "escape")))
  before <- tree_files(fx$src)

  st <- projectData:::stage_source_layout(fx$src)
  expect_true(all(c("data", "extdata") %in% list.files(st$root)))
  expect_no_warning(expect_true(st$close()))
  expect_false(dir.exists(st$root))
  expect_equal(tree_files(fx$src), before)
  expect_identical(readLines(file.path(outside, "precious.txt")), "keep")
  # repeated cleanup is harmless
  expect_no_warning(st$close())
})

test_that("cleanup refuses entries it did not create and keeps a non-empty directory", {
  skip_if_no_links()
  fx <- local_source_pkg()
  local_remove_stage_leftovers()
  st <- projectData:::stage_source_layout(fx$src)
  foreign <- file.path(st$root, "foreign.txt")
  writeLines("not ours", foreign)

  expect_warning(st$close(), class = "projectData_cleanup_warning")
  expect_true(file.exists(foreign))
  expect_false(file.exists(file.path(st$root, "data")))
  expect_true(file.exists(file.path(fx$src, "data", "example.rda")))

  unlink(foreign)
  expect_no_warning(st$close())
  expect_false(dir.exists(st$root))
})

test_that("a link replaced by something else is not removed", {
  skip_if_no_links()
  skip_on_os("windows")
  fx <- local_source_pkg()
  local_remove_stage_leftovers()
  st <- projectData:::stage_source_layout(fx$src)
  link <- file.path(st$root, "data")
  unlink(link, recursive = FALSE)
  dir.create(link)
  writeLines("mine", file.path(link, "mine.txt"))

  expect_warning(st$close(), class = "projectData_cleanup_warning")
  expect_true(file.exists(file.path(link, "mine.txt")))
  expect_true(file.exists(file.path(fx$src, "data", "example.rda")))
  unlink(link, recursive = TRUE)
  expect_no_warning(st$close())
  expect_false(dir.exists(st$root))
})

test_that("a link that cannot be removed is reported without hiding the authoring error", {
  skip_if_no_links()
  local_clean_session()
  fx <- local_source_pkg()
  local_remove_stage_leftovers()
  rec <- local_link_failure(fail_from = 2L)
  testthat::local_mocked_bindings(remove_link = function(path) FALSE)

  got <- capture_error_and_warnings(data_manifest_draft(fx$src))
  expect_s3_class(got$error, "projectData_staging_error")
  expect_match(conditionMessage(got$error), "No data was copied", fixed = TRUE)
  expect_length(got$warnings, 1L)
  expect_s3_class(got$warnings[[1]], "projectData_cleanup_warning")
  expect_match(conditionMessage(got$warnings[[1]]), "projectData-stage-", fixed = TRUE)
  expect_equal(rec$copies, 0L)
  expect_true(file.exists(file.path(fx$src, "data", "example.rda")))
})

test_that("a staging directory that cannot be removed is reported without hiding the error", {
  local_clean_session()
  fx <- local_source_pkg()
  local_remove_stage_leftovers()
  rec <- local_link_failure()
  testthat::local_mocked_bindings(remove_empty_dir = function(path) FALSE)

  got <- capture_error_and_warnings(data_manifest_draft(fx$src))
  expect_s3_class(got$error, "projectData_staging_error")
  expect_length(got$warnings, 1L)
  expect_s3_class(got$warnings[[1]], "projectData_cleanup_warning")
  expect_equal(rec$copies, 0L)
  expect_length(list.files(file.path(tempdir(), stage_dirs()), all.files = TRUE, no.. = TRUE), 0L)
})

test_that("after a failed cleanup of a successful view, a retry removes what remains", {
  skip_if_no_links()
  fx <- local_source_pkg()
  local_remove_stage_leftovers()
  view <- projectData:::authoring_view(fx$src)
  testthat::with_mocked_bindings(
    expect_warning(view$close(), class = "projectData_cleanup_warning"),
    remove_link = function(path) FALSE
  )
  expect_true(dir.exists(view$root))
  expect_no_warning(view$close())
  expect_false(dir.exists(view$root))
  expect_true(file.exists(file.path(fx$src, "data", "example.rda")))
})
