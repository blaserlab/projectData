# Manifest-wide rules shared by reading, standalone validation and writing
# (revision cycle 2, issue 2.3).

local_manifest <- function(inst, ...) {
  file <- file.path(inst, "projectData", "manifest.dcf")
  dir.create(dirname(file), showWarnings = FALSE)
  writeLines(c("ManifestVersion: 1", "", ...), file)
  file
}

man_rec <- function(id, objects = id, source = paste0("extdata/", id, ".tinymat"),
                    loader = "tinydisk::tinymat", options = NULL) {
  c(paste("Resource:", id), paste("Loader:", loader), paste("Source:", source),
    paste("Objects:", objects), if (!is.null(options)) paste("Options:", options), "")
}

man_desc <- function(id, objects = id, ...) {
  modifyList(list(resource = id, loader = "tinydisk::tinymat",
                  source = paste0("extdata/", id, ".tinymat"), objects = objects), list(...))
}

write_quiet <- function(...) suppressMessages(data_manifest_write(...))

test_that("the same object in two resources fails validation and writing alike", {
  local_clean_session()
  inst <- local_inst_tree()
  file <- local_manifest(inst, man_rec("alpha", "shared"), man_rec("bmat", "shared"))
  expect_error(data_manifest_validate(file), "'shared'.*'alpha', 'bmat'|'alpha', 'bmat'",
               class = "projectData_duplicate_object")

  out <- file.path(inst, "out", "manifest.dcf")
  expect_error(write_quiet(list(man_desc("alpha", "shared"), man_desc("bmat", "shared")), out),
               "'alpha', 'bmat'", class = "projectData_duplicate_object")
  expect_false(dir.exists(dirname(out)))
  # Different names are fine, as is a name unknown for one of them.
  expect_no_error(data_manifest_validate(local_manifest(inst, man_rec("alpha", "a"), man_rec("bmat", "b"))))
})

test_that("duplicate resource records fail the same way, agreeing or not", {
  local_clean_session()
  inst <- local_inst_tree()
  same <- local_manifest(inst, man_rec("alpha"), man_rec("alpha"))
  expect_error(data_manifest_validate(same), "duplicate Resource 'alpha'", class = "projectData_manifest_error")
  other <- local_manifest(inst, man_rec("alpha"), man_rec("alpha", "other", loader = "tinydisk::second"))
  expect_error(data_manifest_validate(other), "duplicate Resource 'alpha'.*tinydisk::second",
               class = "projectData_manifest_error")
  out <- file.path(inst, "out", "manifest.dcf")
  expect_error(write_quiet(list(man_desc("alpha"), man_desc("alpha")), out),
               "duplicate Resource 'alpha'", class = "projectData_manifest_error")
  expect_false(dir.exists(dirname(out)))
})

test_that("repeated, empty and malformed option entries fail clearly", {
  local_clean_session()
  inst <- local_inst_tree()
  v <- function(opts) data_manifest_validate(local_manifest(inst, man_rec("alpha", options = opts)))
  expect_error(v("work_copy=true;work_copy=false"), "duplicate Options key.*work_copy.*alpha|alpha.*work_copy",
               class = "projectData_manifest_error")
  expect_error(v("work_copy=true; work_copy=false"), "work_copy")
  expect_error(v("work_copy = true; work_copy=false"), "work_copy")
  expect_error(v("=x"), "empty key")
  expect_error(v(" =x"), "empty key")
  expect_error(v("novalue"), "malformed Options entry \"novalue\"")
  expect_error(v("k="), "empty value")
  expect_error(v("a=1;;b=2"), "empty entry")
  expect_error(v("a=1;"), "empty entry")
  expect_error(v("a=1;b"), "malformed")
  expect_equal(v("a=1;b=2")$alpha, NULL)
  expect_equal(v("a = 1 ; b=2")[[1]]$options, list(a = "1", b = "2"))

  out <- file.path(inst, "out", "manifest.dcf")
  w <- function(opts) write_quiet(list(man_desc("alpha", options = opts)), out)
  expect_error(w(list(k = "a", k = "b")), "duplicate option key.*'k'", class = "projectData_manifest_error")
  expect_error(w(list(k = "a", "b")), "option names")
  expect_error(w(structure(list("a"), names = "")), "option names|empty")
  expect_false(dir.exists(dirname(out)))
})

test_that("duplicate or empty option keys in programmatic descriptors fail validation of descriptors", {
  d <- man_desc("alpha")
  root <- local_inst_tree()
  ctx <- list(collection = NA, package = NA, version = NA, library = NA, root = root, resource = NA)
  d$options <- structure(list("a", "b"), names = c("k", " k"))
  expect_error(projectData:::check_descriptor(d, root, ctx), "duplicate option key")
  d$options <- structure(list("a"), names = " ")
  expect_error(projectData:::check_descriptor(d, root, ctx), "option names cannot be empty")
})

test_that("values containing '=' survive writing and reading", {
  local_clean_session()
  inst <- local_inst_tree()
  out <- file.path(inst, "projectData", "manifest.dcf")
  d <- man_desc("alpha", options = list(token = "a=b==", expr = "x=1=2", plain = "ok"))
  write_quiet(list(d), out)
  expect_equal(data_manifest_validate(out)[[1]]$options,
               list(token = "a=b==", expr = "x=1=2", plain = "ok"))
  hand <- local_manifest(inst, man_rec("alpha", options = "token=abc==; k==v"))
  expect_equal(data_manifest_validate(hand)[[1]]$options, list(token = "abc==", k = "=v"))
})

test_that("valid manifests round-trip without changing descriptors", {
  local_clean_session()
  inst <- local_inst_tree()
  out <- file.path(inst, "projectData", "manifest.dcf")
  ds <- list(man_desc("alpha", c("a1", "a2"), title = "Alpha", options = list(work_copy = "true", n = "1")),
             man_desc("bmat", adapter = list(package = "tinydiskAdapter")))
  write_quiet(ds, out)
  back <- data_manifest_validate(out)
  expect_equal(vapply(back, function(x) x$resource, ""), c("alpha", "bmat"))
  expect_equal(back[[1]]$options, list(work_copy = "true", n = "1"))
  expect_equal(back[[1]]$objects, c("a1", "a2"))
  write_quiet(back, file.path(inst, "second.dcf"), root = inst)
  expect_identical(readLines(file.path(inst, "second.dcf")), readLines(out))
  expect_identical(data_manifest_validate(file.path(inst, "second.dcf"), root = inst), back)
})

test_that("a missing loader does not block structural validation or open anything", {
  local_clean_session()
  inst <- local_inst_tree()
  file <- local_manifest(inst, man_rec("alpha"), man_rec("bmat"))
  cl <- counting_tinydisk()
  expect_false("tinydisk::tinymat" %in% data_loaders()$id)
  expect_no_error(back <- data_manifest_validate(file))
  expect_length(back, 2L)
  expect_equal(cl$counts$open, 0L)
  # A rejected structure is still reported without the loader.
  bad <- local_manifest(inst, man_rec("alpha", "x"), man_rec("bmat", "x"))
  expect_error(data_manifest_validate(bad), class = "projectData_duplicate_object")
  expect_false("tinydisk::tinymat" %in% data_loaders()$id)
  # Registered adapter: validation runs but never opens.
  data_loader_add(cl$loader)
  file <- local_manifest(inst, man_rec("alpha"), man_rec("bmat"))
  data_manifest_validate(file)
  expect_equal(cl$counts$open, 0L)
})

test_that("rejected writes leave existing manifests unchanged and no partial output", {
  local_clean_session()
  inst <- local_inst_tree()
  out <- file.path(inst, "projectData", "manifest.dcf")
  write_quiet(list(man_desc("alpha")), out)
  before <- readLines(out)
  bad <- list(
    list(man_desc("alpha", "s"), man_desc("bmat", "s")),
    list(man_desc("alpha"), man_desc("alpha")),
    list(man_desc("alpha", options = list(k = "1", k = "2")))
  )
  for (b in bad) expect_error(write_quiet(b, out, overwrite = TRUE))
  expect_identical(readLines(out), before)
  expect_equal(list.files(dirname(out), all.files = TRUE, no.. = TRUE), "manifest.dcf")
  new <- file.path(inst, "fresh", "sub", "manifest.dcf")
  expect_error(write_quiet(bad[[1]], new))
  expect_false(dir.exists(file.path(inst, "fresh")))
})

test_that("a validated manifest assembles; identical names in separate collections stay valid", {
  local_clean_session()
  lib <- fixture_library()
  files <- list("extdata/alpha.tinymat" = function(p) tinymat_write(diag(2), p),
                "extdata/bmat.tinymat" = function(p) tinymat_write(diag(2), p))
  mf <- c("ManifestVersion: 1", "", man_rec("alpha", "same_name"), man_rec("bmat", "other"))
  fixture_package(lib, "pdrules1", lazy = FALSE, files = files, manifest = mf)
  fixture_package(lib, "pdrules2", lazy = FALSE, files = files, manifest = mf)
  data_loader_add(tinydisk_loader())
  data_attach(c("pdrules1", "pdrules2"), lib.loc = lib)
  expect_equal(sum(data_catalog()$object == "same_name"), 2L)
})

test_that("source-tree and installed layouts from issue 2.1 still work", {
  local_clean_session()
  lib <- fixture_library()
  src <- file.path(withr::local_tempdir(), "pdlayout")
  dir.create(file.path(src, "inst", "extdata"), recursive = TRUE)
  dir.create(file.path(src, "data"))
  writeLines(c("Package: pdlayout", "Version: 0.1.0", "Title: x"), file.path(src, "DESCRIPTION"))
  x <- 1
  save(x, file = file.path(src, "data", "example.rda"))
  tinymat_write(diag(2), file.path(src, "inst", "extdata", "alpha.tinymat"))
  ds <- list(man_desc("alpha"),
             list(resource = "example", loader = "projectData::dataset", source = "data/example.rda",
                  objects = "x"))
  out <- file.path(src, "inst", "projectData", "manifest.dcf")

  if (links_supported()) {
    write_quiet(ds, out)
    expect_length(data_manifest_validate(out), 2L)
    expect_error(write_quiet(list(ds[[1]], man_desc("alpha")), out, overwrite = TRUE), "duplicate Resource")
    expect_length(data_manifest_validate(out), 2L)

    fixture_package(lib, "pdinst2", lazy = FALSE,
                    files = list("extdata/alpha.tinymat" = function(p) tinymat_write(diag(2), p)),
                    manifest = readLines(out)[c(1:6)])
    installed <- file.path(lib, "pdinst2", "projectData", "manifest.dcf")
    expect_length(data_manifest_validate(installed, root = file.path(lib, "pdinst2")), 1L)
  } else {
    # Without directory-link support, authoring against this source tree
    # cannot stage the installed-shaped view it needs and must fail clearly,
    # without copying data or leaving any output behind.
    rec <- local_link_failure()
    err <- expect_error(write_quiet(ds, out), class = "projectData_staging_error")
    expect_match(conditionMessage(err), "No data was copied", fixed = TRUE)
    expect_equal(rec$copies, 0L)
    expect_false(dir.exists(dirname(out)))
    expect_length(stage_dirs(), 0L)

    # The installed-layout path does not need staging and still works.
    fixture_package(lib, "pdinst2", lazy = FALSE,
                    files = list("extdata/alpha.tinymat" = function(p) tinymat_write(diag(2), p)),
                    manifest = c("ManifestVersion: 1", "", man_rec("alpha")))
    installed <- file.path(lib, "pdinst2", "projectData", "manifest.dcf")
    expect_length(data_manifest_validate(installed, root = file.path(lib, "pdinst2")), 1L)
  }
})
