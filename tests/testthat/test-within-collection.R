# Object names are unique across resources within one collection (issue 2.2).

# cohort_a records sample_metadata; cohort_b has an unknown mapping (no index
# row) but actually produces the same name.
dup_library <- function(extra = list(), extra_manifest = NULL, env = parent.frame()) {
  lib <- fixture_library(env)
  fixture_package(lib, "pddup", "0.1.0", lazy = FALSE,
    datasets = c(list(
      cohort_a = list(sample_metadata = "from-a"),
      cohort_b = list(sample_metadata = "from-b"),
      other = list(other_obj = "o")), extra),
    drop_index = "cohort_b")
  lib
}

collision_snapshot <- function() {
  list(cat = data_catalog(), prefs = data_preferences(), cols = data_collections(),
       env = if (!is.na(match("projectData:data", search()))) ls("projectData:data") else NULL)
}

test_that("inspection cannot add a duplicate name (attached collection)", {
  lib <- dup_library()
  local_clean_session()
  n <- local_open_counter()
  data_attach("pddup", lib.loc = lib, id = "dup")
  expect_true(any(data_catalog()$mapping == "unknown"))
  before <- collision_snapshot()
  expect_identical(data_get("sample_metadata"), "from-a")

  err <- expect_error(suppressWarnings(data_inspect("dup", "cohort_b")),
                      class = "projectData_duplicate_object")
  expect_match(conditionMessage(err), "'sample_metadata'")
  expect_match(conditionMessage(err), "collection 'dup'")
  expect_match(conditionMessage(err), "'cohort_a', 'cohort_b'")

  after <- collision_snapshot()
  # state/mapping of the opened resource legitimately report the cached open
  cols <- c("resource", "object", "binding", "exposed_name")
  expect_identical(after$cat[, cols], before$cat[, cols])
  expect_identical(after$prefs, before$prefs)
  expect_identical(after$cols, before$cols)
  expect_identical(after$env, before$env)
  expect_identical(data_get("sample_metadata"), "from-a")
  expect_equal(sum(data_catalog()$object == "sample_metadata", na.rm = TRUE), 1)
})

test_that("rejection keeps opened resources cached and is not a loader failure", {
  lib <- dup_library()
  local_clean_session()
  n <- local_open_counter()
  data_attach("pddup", lib.loc = lib, id = "dup")
  for (i in 1:2) {
    expect_error(suppressWarnings(data_inspect("dup", "cohort_b")),
                 class = "projectData_duplicate_object")
  }
  cat <- data_catalog()
  expect_equal(cat$state[cat$resource == "cohort_b"], "ready")
  expect_equal(cat$mapping[cat$resource == "cohort_b"], "unknown")
  # opened once despite two inspections
  expect_equal(n$log, "dup/cohort_b")
})

test_that("the same rule holds for an unattached collection from data_get(package =)", {
  lib <- dup_library()
  local_clean_session()
  n <- local_open_counter()
  expect_identical(data_get("other_obj", package = "pddup", lib.loc = lib), "o")
  expect_false(data_collections()$attached)
  before <- data_catalog("pddup")
  expect_error(suppressWarnings(data_inspect("pddup", "cohort_b")),
               class = "projectData_duplicate_object")
  expect_identical(data_catalog("pddup")[, c("resource", "object", "mapping")],
                   before[, c("resource", "object", "mapping")])
  expect_identical(data_get("sample_metadata", package = "pddup"), "from-a")
  expect_false(data_collections()$attached)
  expect_equal(sum(n$log == "pddup/cohort_b"), 1)
})

test_that("correcting a wrong recorded mapping to an owned name is rejected", {
  lib <- fixture_library()
  fixture_package(lib, "pdwrong", "0.1.0", lazy = FALSE,
    datasets = list(a = list(sample_metadata = "from-a"),
                    b = list(sample_metadata = "from-b")),
    manifest = c("ManifestVersion: 1", "",
                 "Resource: b", "Loader: projectData::dataset",
                 "Source: data/b.rda", "Objects: wrongly_named"))
  local_clean_session()
  n <- local_open_counter()
  data_attach("pdwrong", lib.loc = lib, id = "w")
  before <- data_catalog()
  expect_error(suppressWarnings(data_inspect("w", "b")), class = "projectData_duplicate_object")
  expect_identical(data_catalog()[, c("resource", "object", "binding")],
                   before[, c("resource", "object", "binding")])
  expect_equal(data_catalog()$object[data_catalog()$resource == "b"], "wrongly_named")
  expect_identical(data_get("sample_metadata"), "from-a")
  expect_equal(n$log, c("w/b", "w/a"))
})

test_that("several resources are validated as one proposed mapping", {
  lib <- fixture_library()
  # a is recorded as 'sample_metadata' but produces 'a_real'; b is unknown and
  # produces 'sample_metadata'. Alone, b collides; together they are consistent.
  fixture_package(lib, "pdswap", "0.1.0", lazy = FALSE,
    datasets = list(a = list(a_real = "A"), b = list(sample_metadata = "B"),
                    c = list(sample_metadata = "C")),
    drop_index = c("b", "c"),
    manifest = c("ManifestVersion: 1", "",
                 "Resource: a", "Loader: projectData::dataset",
                 "Source: data/a.rda", "Objects: sample_metadata"))
  local_clean_session()
  n <- local_open_counter()
  data_attach("pdswap", lib.loc = lib, id = "s")

  # b alone collides with a's (wrong) recorded name
  expect_error(suppressWarnings(data_inspect("s", "b")), class = "projectData_duplicate_object")
  # b and c together are invalid (both produce sample_metadata): nothing commits
  cat0 <- data_catalog()
  expect_error(suppressWarnings(data_inspect("s", c("a", "b", "c"))),
               class = "projectData_duplicate_object")
  expect_identical(data_catalog()[, c("resource", "object", "binding")],
                   cat0[, c("resource", "object", "binding")])
  expect_equal(cat0$object[cat0$resource == "a"], "sample_metadata")

  # a and b together is a valid coordinated correction
  expect_no_error(suppressWarnings(data_inspect("s", c("a", "b"))))
  cat <- data_catalog()
  expect_equal(cat$object[cat$resource == "a"], "a_real")
  expect_equal(cat$object[cat$resource == "b"], "sample_metadata")
  expect_identical(data_get("sample_metadata"), "B")
  expect_equal(sum(n$log == "s/b"), 1)
  expect_equal(sum(n$log == "s/a"), 1)
})

test_that("the same name in separate collections still follows conflict rules", {
  lib <- dup_library()
  local_clean_session()
  fixture_package(lib, "pdelse", "0.1.0", lazy = FALSE,
                  datasets = list(z = list(sample_metadata = "else")))
  data_attach("pdelse", lib.loc = lib, id = "else")
  data_attach("pddup", lib.loc = lib, id = "dup", include = c("cohort_a", "other"))
  cat <- data_catalog()
  expect_equal(cat$binding[which(cat$object == "sample_metadata")], c("ambiguous", "ambiguous"))
  expect_error(data_get("sample_metadata"), class = "projectData_ambiguous")
  data_prefer("sample_metadata", "else")
  expect_identical(data_get("sample_metadata"), "else")
  expect_identical(data_get("sample_metadata", collection = "dup"), "from-a")
})

test_that("initial assembly rejects known duplicates with the same condition", {
  lib <- fixture_library()
  fixture_package(lib, "pdknown", "0.1.0", lazy = FALSE,
    datasets = list(a = list(sample_metadata = "A"), b = list(sample_metadata = "B")))
  local_clean_session()
  err <- expect_error(data_attach("pdknown", lib.loc = lib, id = "k"),
                      class = "projectData_duplicate_object")
  expect_match(conditionMessage(err), "collection 'k'")
  expect_match(conditionMessage(err), "'a', 'b'")
  expect_equal(nrow(data_collections()), 0)
})

test_that("retrieval never silently takes the first of several owners", {
  co <- list(id = "x", objects = data.frame(resource = c("r1", "r2"),
                                            object = c("o", "o")))
  expect_error(projectData:::check_unique_objects("x", co$objects$resource, co$objects$object),
               class = "projectData_duplicate_object")
  expect_silent(projectData:::check_unique_objects("x", c("r1", "r1", "r2"), c("o", "o2", NA)))
})
