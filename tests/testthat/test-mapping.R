# R CMD INSTALL rebuilds the dataset index from the real files, so wrong
# mappings are supplied through a manifest, which is supported input: it
# declares object names for built-in dataset resources that the files contradict.
mismatch_library <- function(env = parent.frame()) {
  lib <- fixture_library(env)
  fixture_package(lib, "pdmis", "0.1.0", lazy = FALSE,
    datasets = list(
      example_set = list(sample_metadata = "meta"),
      pp = list(foo = "foo-value", baz = "baz-value"),
      fine = list(fine_obj = 1)
    ),
    manifest = c(
      "ManifestVersion: 1", "",
      "Resource: example_set", "Loader: projectData::dataset",
      "Source: data/example_set.rda", "Objects: example_set", "",
      "Resource: pp", "Loader: projectData::dataset",
      "Source: data/pp.rda", "Objects: foo, bar"
    ))
  lib
}

test_that("querying and attaching a mismatched package opens nothing", {
  lib <- mismatch_library()
  local_clean_session()
  n <- local_open_counter()

  q <- data_query("pdmis", lib.loc = lib)
  expect_setequal(q$object, c("example_set", "foo", "bar", "fine_obj"))
  data_attach("pdmis", lib.loc = lib, id = "mis")
  data_catalog()
  expect_length(n$log, 0)
  expect_true(all(data_catalog()$state == "unopened"))
})

test_that("complete mismatch: access fails clearly, inspection repairs, retrieval works", {
  lib <- mismatch_library()
  local_clean_session()
  n <- local_open_counter()
  data_attach("pdmis", lib.loc = lib, id = "mis")

  expect_error(data_get("example_set"), class = "projectData_mapping_error")
  expect_error(data_get("example_set"), "'example_set'.*sample_metadata.*data_inspect")
  expect_error(data_get("example_set"), "Resource 'example_set' of collection 'mis'")
  cat <- data_catalog()
  expect_equal(cat$state[cat$resource == "example_set"], "ready")
  expect_equal(cat$mapping[cat$resource == "example_set"], "mismatch")

  expect_no_warning(data_inspect("mis", "example_set"))
  cat <- data_catalog()
  rows <- cat[cat$resource == "example_set", ]
  expect_equal(rows$object, "sample_metadata")
  expect_equal(rows$mapping, "verified")
  expect_equal(rows$binding, "bound")
  expect_false("example_set" %in% cat$exposed_name)
  expect_error(data_get("example_set"), class = "projectData_not_found")
  expect_identical(data_get("sample_metadata"), "meta")
  expect_equal(n$log, "mis/example_set")
})

test_that("partial mismatch reports the full observed set and is not verified by one hit", {
  lib <- mismatch_library()
  local_clean_session()
  data_attach("pdmis", lib.loc = lib, id = "mis")

  expect_identical(data_get("foo"), "foo-value")
  cat <- data_catalog()
  pp <- cat[cat$resource == "pp", ]
  expect_equal(pp$mapping[pp$object == "foo"], "verified")
  expect_equal(pp$mapping[pp$object == "bar"], "mismatch")
  expect_error(data_get("bar"), "'bar'.*'baz'.*'foo'", class = "projectData_mapping_error")

  data_inspect("mis", "pp")
  cat <- data_catalog()
  pp <- cat[cat$resource == "pp", ]
  expect_setequal(pp$object, c("baz", "foo"))
  expect_true(all(pp$mapping == "verified"))
  expect_false("bar" %in% cat$exposed_name)
  expect_identical(data_get("baz"), "baz-value")
})

test_that("inspection and later access share one successful open, including aliases", {
  lib <- mismatch_library()
  local_clean_session()
  n <- local_open_counter()
  data_attach("pdmis", lib.loc = lib, id = "mis", suffix = "_mis")

  suppressWarnings(data_inspect("mis", "pp"))
  expect_identical(data_get("foo"), "foo-value")
  expect_identical(data_get("baz_mis"), "baz-value")
  expect_identical(data_get("baz", collection = "mis"), "baz-value")
  expect_equal(n$log, "mis/pp")

  # a strict failure opens its own resource once, and never reopens pp
  expect_error(data_get("example_set"), class = "projectData_mapping_error")
  expect_error(data_get("example_set"), class = "projectData_mapping_error")
  expect_equal(n$log, c("mis/pp", "mis/example_set"))
})

test_that("genuine opening failure is not a mapping result and keeps retry semantics", {
  lib <- mismatch_library()
  src <- file.path(lib, "pdmis", "data", "example_set.rda")
  backup <- withr::local_tempfile(fileext = ".rda")
  file.copy(src, backup)
  local_clean_session()
  n <- local_open_counter()
  data_attach("pdmis", lib.loc = lib, id = "mis")
  unlink(src)

  expect_error(suppressWarnings(data_inspect("mis", "example_set")),
               class = "projectData_opening_failed")
  cat <- data_catalog()
  expect_equal(cat$state[cat$resource == "example_set"], "failed")
  expect_equal(cat$object[cat$resource == "example_set"], "example_set")
  expect_equal(cat$mapping[cat$resource == "example_set"], "recorded")
  expect_error(suppressWarnings(data_get("example_set")), "data_retry",
               class = "projectData_opening_failed")
  expect_length(n$log, 1)

  file.copy(backup, src)
  data_retry("mis", "example_set")
  suppressWarnings(data_inspect("mis", "example_set"))
  expect_identical(data_get("sample_metadata"), "meta")
  expect_length(n$log, 2)
})

test_that("a repaired name that collides follows the normal conflict rules", {
  lib <- mismatch_library()
  local_clean_session()
  fixture_package(lib, "pdother", "0.1.0", lazy = FALSE,
                  datasets = list(o = list(sample_metadata = "other")))
  data_attach("pdother", lib.loc = lib, id = "other")
  data_attach("pdmis", lib.loc = lib, id = "mis")
  suppressWarnings(data_inspect("mis", "example_set"))

  cat <- data_catalog()
  expect_equal(cat$binding[cat$object == "sample_metadata"], c("ambiguous", "ambiguous"))
  expect_error(data_get("sample_metadata"), class = "projectData_ambiguous")
  expect_identical(data_get("sample_metadata", collection = "mis"), "meta")
  data_prefer("sample_metadata", "other")
  expect_identical(data_get("sample_metadata"), "other")
})
