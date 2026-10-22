# A source-tree `inst/` with two tinymat resources.
local_inst_tree <- function(env = parent.frame()) {
  inst <- withr::local_tempdir(.local_envir = env)
  for (nm in c("alpha", "bmat")) {
    tinymat_write(matrix(as.double(1:6), 2, 3), file.path(inst, "extdata", paste0(nm, ".tinymat")))
  }
  inst
}

counting_tinydisk <- function(id = "tinydisk::tinymat") {
  counts <- new.env()
  counts$discover <- 0L
  counts$open <- 0L
  l <- tinydisk_loader()
  l$id <- id
  d <- l$discover
  o <- l$open
  l$discover <- function(root, context) { counts$discover <- counts$discover + 1L; d(root, context) }
  l$open <- function(descriptor, paths, context) { counts$open <- counts$open + 1L; o(descriptor, paths, context) }
  list(loader = l, counts = counts)
}

test_that("draft returns adapter claims from metadata only, sorted", {
  local_clean_session()
  inst <- local_inst_tree()
  cl <- counting_tinydisk()
  data_loader_add(cl$loader)
  d <- data_manifest_draft(inst)
  expect_equal(vapply(d, function(x) x$resource, ""), c("alpha", "bmat"))
  expect_equal(d[[1]]$source, "extdata/alpha.tinymat")
  expect_equal(nrow(attr(d, "conflicts")), 0L)
  expect_equal(c(cl$counts$discover, cl$counts$open), c(1L, 0L))
  expect_false(exists("alpha", envir = globalenv(), inherits = FALSE))
})

test_that("draft requires pre-registered adapters and registers nothing", {
  local_clean_session()
  inst <- local_inst_tree()
  expect_error(data_manifest_draft(inst), class = "projectData_no_loaders")
  expect_error(data_manifest_draft(inst, loaders = "tinydisk::tinymat"),
               class = "projectData_loader_unavailable")
  data_loader_add(data_loader("test::nodiscover", open = function(descriptor, paths, context) list()))
  expect_error(data_manifest_draft(inst, loaders = "test::nodiscover"), class = "projectData_no_loaders")
  expect_error(data_manifest_draft(inst), class = "projectData_no_loaders")
  expect_false("tinydisk::tinymat" %in% data_loaders()$id)
  expect_error(data_manifest_draft(file.path(inst, "nope")), "existing directory")
})

test_that("ambiguous claims are reported and resolved by naming the loader", {
  local_clean_session()
  inst <- local_inst_tree()
  data_loader_add(counting_tinydisk("tinydisk::tinymat")$loader)
  data_loader_add(counting_tinydisk("tinydisk::second")$loader)
  expect_warning(d <- data_manifest_draft(inst), class = "projectData_draft_conflict")
  expect_length(d, 0L)
  cf <- attr(d, "conflicts")
  expect_equal(sort(unique(cf$resource)), c("alpha", "bmat"))
  expect_equal(unique(cf$reason), "claimed more than once")
  expect_no_warning(d <- data_manifest_draft(inst, loaders = "tinydisk::second"))
  expect_equal(unique(vapply(d, function(x) x$loader, "")), "tinydisk::second")
  expect_length(d, 2L)
})

test_that("claims on standard datasets are left out unless the loader is named", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdclaim", lazy = FALSE, datasets = list(plain = list(plain_obj = 1)))
  data_loader_add(data_loader("test::claim", open = function(descriptor, paths, context) list(),
    discover = function(root, context) list(list(resource = "plain", objects = "plain_obj"))))
  root <- file.path(lib, "pdclaim")
  expect_warning(d <- data_manifest_draft(root), class = "projectData_draft_conflict")
  expect_length(d, 0L)
  expect_match(attr(d, "conflicts")$reason, "standard dataset")
  d <- data_manifest_draft(root, loaders = "test::claim")
  expect_equal(d[[1]]$resource, "plain")
  expect_equal(d[[1]]$loader, "test::claim")
})

test_that("a drafted manifest round-trips, validates and drives the catalog", {
  local_clean_session()
  inst <- local_inst_tree()
  cl <- counting_tinydisk()
  data_loader_add(cl$loader)
  d <- data_manifest_draft(inst) |>
    lapply(\(x) { x$options <- list(work_copy = "true"); x$title <- "A tiny matrix"
                  x$adapter <- list(package = "tinydiskAdapter"); x })
  file <- file.path(inst, "projectData", "manifest.dcf")
  res <- data_manifest_write(d, file)
  expect_equal(res$loader_validation, c("ran", "ran"))
  expect_identical(attr(res, "file"), file)
  expect_no_error(back <- data_manifest_validate(file))
  expect_equal(back[[1]]$options, list(work_copy = "true"))
  expect_equal(back[[1]]$adapter, list(package = "tinydiskAdapter", export = "projectData_loaders"))
  expect_false(any(grepl("AdapterExport", readLines(file))))
  expect_equal(cl$counts$open, 0L)

  lib <- fixture_library()
  fixture_package(lib, "pdwritten", lazy = FALSE,
    files = list("extdata/alpha.tinymat" = function(p) tinymat_write(matrix(as.double(1:6), 2, 3), p),
                 "extdata/bmat.tinymat" = function(p) tinymat_write(matrix(as.double(1:6), 2, 3), p)),
    manifest = readLines(file))
  data_attach("pdwritten", lib.loc = lib)
  cat <- data_catalog()
  expect_equal(cat$origin[cat$object == "alpha"], "manifest")
  expect_equal(cat$title[cat$object == "alpha"], "A tiny matrix")
})

test_that("skipped loader validation is reported", {
  local_clean_session()
  inst <- local_inst_tree()
  file <- file.path(inst, "projectData", "manifest.dcf")
  d <- list(resource = "alpha", loader = "tinydisk::tinymat", source = "extdata/alpha.tinymat",
            objects = "alpha")
  expect_message(res <- data_manifest_write(d, file), class = "projectData_validation_skipped")
  expect_equal(res$loader_validation, "skipped: loader not registered")
  expect_true(file.exists(file))

  data_loader_add(tinydisk_loader())
  expect_no_message(res <- data_manifest_write(d, file, overwrite = TRUE))
  expect_equal(res$loader_validation, "ran")

  # Registered loader validation is applied before anything is written.
  unlink(file.path(inst, "extdata", "alpha.tinymat", "values.bin"))
  expect_error(data_manifest_write(d, file, overwrite = TRUE), "missing dim.txt or values.bin")
})

test_that("anything a manifest cannot hold is an error", {
  local_clean_session()
  inst <- local_inst_tree()
  file <- file.path(inst, "projectData", "manifest.dcf")
  base <- list(resource = "alpha", loader = "tinydisk::tinymat", source = "extdata/alpha.tinymat",
               objects = c("a1", "a2"))
  w <- function(...) suppressMessages(data_manifest_write(modifyList(base, list(...)), file))
  expect_error(w(title = c("x", "y")), "differs between objects", class = "projectData_manifest_error")
  expect_error(w(options = list(k = "a;b")), "cannot contain ';'")
  expect_error(w(options = list(`k=` = "a")), "cannot contain")
  expect_error(w(options = list(k = TRUE)), "single character")
  expect_error(w(options = list(k = "")), "non-empty")
  expect_error(w(objects = c("a,b", "c")), "comma")
  expect_error(w(title = "two\nlines"), "line break")
  expect_error(w(title = " padded"), "whitespace")
  expect_error(w(source = "../x"), "relative path")
  expect_error(w(source = "extdata/none.tinymat"), "does not exist")
  expect_error(w(loader = "nonamespace"), "namespaced")
  expect_error(w(extra = 1), "unknown field")
  expect_error(w(adapter = list(package = "p", bogus = 1)), "unknown element")
  expect_error(data_manifest_write(list(base, base), file), "more than once")
  expect_error(suppressMessages(data_manifest_write(list(base, modifyList(base, list(resource = "bmat"))), file)),
               class = "projectData_duplicate_object")
  expect_false(file.exists(file))
  expect_false(dir.exists(dirname(file)))
  # Identical per-object values are accepted.
  expect_no_error(w(title = c("same", "same")))
  expect_equal(data_manifest_validate(file)[[1]]$title, c("same", "same"))
})

test_that("files are never clobbered, partially written or put in installed packages", {
  local_clean_session()
  inst <- local_inst_tree()
  file <- file.path(inst, "projectData", "manifest.dcf")
  d <- list(resource = "alpha", loader = "tinydisk::tinymat", source = "extdata/alpha.tinymat",
            objects = "alpha")
  suppressMessages(data_manifest_write(d, file))
  before <- readLines(file)
  expect_error(suppressMessages(data_manifest_write(d, file)), "already exists")
  expect_error(suppressMessages(data_manifest_write(modifyList(d, list(source = "extdata/none")),
                                                    file, overwrite = TRUE)), "does not exist")
  expect_identical(readLines(file), before)
  expect_equal(list.files(dirname(file), all.files = TRUE, no.. = TRUE), "manifest.dcf")

  lib <- fixture_library()
  fixture_package(lib, "pdinstalled", lazy = FALSE, datasets = list(plain = list(plain_obj = 1)))
  target <- file.path(lib, "pdinstalled", "projectData", "manifest.dcf")
  expect_error(data_manifest_write(list(), target, root = file.path(lib, "pdinstalled")),
               "installed package")
  expect_false(file.exists(target))
})
