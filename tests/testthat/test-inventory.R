test_that("inventory, registration, catalog printing and conflict handling open nothing", {
  fx <- shared_fixtures()
  local_clean_session()
  n <- local_open_counter()

  data_register(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "ref"),
                suffix = c(NA, ".reference"))
  cat <- data_catalog()
  expect_output(print(cat), "metadata only")
  data_info("cds_main")
  data_prefer("cds_main", "main")
  data_primary("main")
  data_prefer("cds_main", NULL)
  data_attach()
  data_detach()
  expect_length(n$log, 0)
  expect_true(all(cat$state %in% c("unopened", "unavailable-loader")))
})

test_that("lazy and non-lazy datasets are inventoried, with multi-object resources", {
  fx <- shared_fixtures()
  local_clean_session()
  expect_true(file.exists(file.path(fx$lib, "pdmain", "data", "Rdata.rdb")))
  expect_false(file.exists(file.path(fx$lib, "pdref", "data", "Rdata.rdb")))
  expect_true(file.exists(file.path(fx$lib, "pdref", "data", "multi.rda")))

  data_register(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "ref"))
  cat <- data_catalog()
  pair <- cat[cat$resource == "pair" & cat$collection == "main", ]
  expect_setequal(pair$object, c("pair_x", "pair_y"))
  multi <- cat[cat$resource == "multi" & cat$collection == "ref", ]
  expect_setequal(multi$object, c("m1", "m2"))
  expect_true(all(cat$mapping[cat$resource %in% c("pair", "multi")] == "index"))
  # original dataset identifiers are preserved and differ from object names
  expect_false("multi" %in% cat$object)
})

test_that("a multi-object resource is opened once however it is reached", {
  fx <- shared_fixtures()
  local_clean_session()
  n <- local_open_counter()
  data_register("pdref", lib.loc = fx$lib, suffix = ".r")
  data_attach()
  expect_identical(data_get("m1"), "m1")
  expect_identical(data_get("m1.r"), "m1")
  expect_identical(data_get("m2", "pdref"), "m2")
  expect_identical(get("m2", envir = as.environment("projectData:data")), "m2")
  expect_identical(get("m1", envir = data_env("pdref")), "m1")
  expect_equal(n$log, "pdref/multi")
  cat <- data_catalog()
  expect_equal(unique(cat$state[cat$resource == "multi"]), "ready")
  expect_true(all(cat$mapping[cat$resource == "multi"] == "verified"))
})

test_that("the intended installation is selected regardless of later .libPaths()", {
  fx <- shared_fixtures()
  local_clean_session()
  data_register("pdmain", lib.loc = fx$lib, id = "v1")
  expect_equal(data_collections()$version, "0.1.0")
  .libPaths(c(fx$lib2, .libPaths()))
  expect_identical(data_get("cds_main", "v1"), "main-v1")

  data_register("pdmain", id = "v2")
  expect_equal(data_collections()$version[data_collections()$id == "v2"], "0.2.0")
  expect_identical(data_get("cds_main", "v2"), "main-v2")
  expect_error(data_register("pdmain", lib.loc = file.path(tempdir(), "nowhere"), id = "z"),
               class = "projectData_not_installed")
})

test_that("incomplete mappings stay unknown until explicitly inspected", {
  fx <- shared_fixtures()
  local_clean_session()
  n <- local_open_counter()
  data_register("pdref", lib.loc = fx$lib)
  cat <- data_catalog()
  undoc <- cat[cat$resource == "undoc", ]
  expect_equal(undoc$mapping, "unknown")
  expect_equal(undoc$binding, "unmapped")
  expect_true(is.na(undoc$object))
  expect_error(data_get("hidden"), class = "projectData_not_found")
  expect_length(n$log, 0)

  expect_warning(data_inspect("pdref"), class = "projectData_inspection")
  expect_equal(n$log, "pdref/undoc")
  cat <- data_catalog()
  expect_equal(cat$mapping[cat$object %in% "hidden"], "verified")
  expect_identical(data_get("hidden"), 5)
  expect_equal(n$log, "pdref/undoc")
})

test_that("the datasets package is inventoried by its index conventions without opening data", {
  skip_if_not(file.exists(file.path(system.file(package = "datasets"), "Meta", "data.rds")))
  local_clean_session()
  n <- local_open_counter()
  data_register("datasets")
  cat <- data_catalog()
  b <- cat[cat$object %in% c("beaver1", "beaver2") & cat$kind == "original", ]
  expect_equal(unique(b$resource), "beavers")
  expect_length(n$log, 0)
})

test_that("global objects are never overwritten and shadow the facade with a warning", {
  fx <- shared_fixtures()
  local_clean_session()
  assign("only_ref", "global value", envir = globalenv())
  withr::defer(rm("only_ref", envir = globalenv()))
  data_register("pdref", lib.loc = fx$lib)
  expect_warning(data_attach(), class = "projectData_masking")
  expect_identical(get("only_ref", envir = globalenv()), "global value")
  expect_identical(only_ref, "global value")
  expect_identical(data_get("only_ref"), 3)
  expect_true("only_ref" %in% data_masking()$name)
  expect_error(data_detach(), NA)
  expect_error(data_attach(on_mask = "error"), class = "projectData_masking")
  expect_false("projectData:data" %in% search())
})
