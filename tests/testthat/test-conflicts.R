two_collections <- function(suffix = c(NA, ".reference"), primary = NULL) {
  fx <- shared_fixtures()
  data_attach(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "reference"),
                suffix = suffix, primary = primary)
}

test_that("an unresolved conflict is visible in the catalog and fails before returning data", {
  local_clean_session()
  n <- local_open_counter()
  two_collections()
  cat <- data_catalog()
  expect_equal(sort(cat$collection[cat$exposed_name %in% "cds_main" & cat$binding %in% "ambiguous"]),
               c("main", "reference"))
  expect_error(data_get("cds_main"), class = "projectData_ambiguous")
  expect_error(get("cds_main", envir = as.environment("projectData:data")),
               "ambiguous", class = "projectData_ambiguous")
  expect_identical(data_get("cds_main.reference"), "ref")
  expect_identical(data_get("cds_main", collection = "main"), "main-v1")
  expect_equal(sort(n$log), c("main/cds_main", "reference/cds_main"))
})

test_that("an explicit primary keeps bare names; alias and qualified access still work", {
  local_clean_session()
  two_collections(primary = "main")
  expect_identical(cds_main, "main-v1")
  expect_identical(cds_main.reference, "ref")
  cat <- data_catalog()
  expect_equal(cat$binding[cat$collection == "reference" & cat$exposed_name %in% "cds_main"], "not_bound")
  expect_equal(data_primary(), "main")
})

test_that("preferences override the primary and can be cleared", {
  local_clean_session()
  two_collections(primary = "main")
  data_prefer("cds_main", "reference")
  expect_identical(cds_main, "ref")
  data_prefer("cds_main", NULL)
  expect_identical(cds_main, "main-v1")
  expect_error(data_prefer("cds_main", "nonexistent"), class = "projectData_bad_preference")
  expect_error(data_prefer("meta_a", "reference"), class = "projectData_bad_preference")
  expect_equal(nrow(data_preferences()), 0)
})

test_that("no winner is chosen by attachment order", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib, id = "reference")
  expect_identical(data_get("cds_main"), "ref")
  data_attach("pdmain", lib.loc = fx$lib, id = "main")
  expect_error(data_get("cds_main"), class = "projectData_ambiguous")
})

test_that("failed validation commits nothing", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdmain", lib.loc = fx$lib, id = "main", suffix = ".x")
  before <- list(data_collections(), sort(ls(as.environment("projectData:data"))), data_catalog())
  expect_error(data_attach("pdref", lib.loc = fx$lib, id = "other", suffix = ".x"),
               class = "projectData_name_collision")
  expect_error(data_attach("pdmain", lib.loc = fx$lib, id = "main"), "already attached")
  expect_error(data_attach("pdref", lib.loc = fx$lib, id = "o", include = "nope"), "include")
  expect_error(data_attach(c("pdref", "pdmain"), lib.loc = fx$lib, id = c("o", "o")), "unique")
  expect_error(data_attach("pdref", lib.loc = fx$lib, id = "o", primary = "ghost"), "not attached")
  after <- list(data_collections(), sort(ls(as.environment("projectData:data"))), data_catalog())
  expect_identical(before, after)
})

test_that("a bad alias against an existing collection is caught by name only", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_attach("pdmain", lib.loc = fx$lib, id = "main")
  # suffix makes meta_a + "" impossible; an alias equal to an existing name fails
  expect_error(data_attach("pdmain", lib.loc = fx$lib, id = "again", suffix = ""),
               "non-empty")
  expect_length(n$log, 0)
})

test_that("include restricts exposure, and siblings share the resource", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_attach("pdmain", lib.loc = fx$lib, include = "pair_x")
  cat <- data_catalog()
  expect_equal(cat$binding[cat$object %in% "pair_y"], "excluded")
  expect_false("pair_y" %in% ls(as.environment("projectData:data")))
  expect_identical(pair_x, "x")
  expect_error(data_get("pair_y"), class = "projectData_not_found")
  expect_length(n$log, 1)
})

test_that("reserved names cannot be exposed", {
  local_clean_session()
  fx <- shared_fixtures()
  tmp <- fixture_library()
  fixture_package(tmp, "pdbad", lazy = TRUE, datasets = list(res = list(.Random.seed = 1)), documented = list(res = character()))
  expect_error(data_attach("pdbad", lib.loc = tmp), class = "projectData_reserved_name")
  expect_equal(nrow(data_collections()), 0)
})
