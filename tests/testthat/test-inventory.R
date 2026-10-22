test_that("data_query() is stateless and opens nothing", {
  fx <- shared_fixtures()
  local_clean_session()
  n <- local_open_counter()
  globals <- ls(globalenv(), all.names = TRUE)
  sp <- search()
  opts <- options()

  q <- data_query(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "ref"),
                  suffix = c(NA, ".reference"))
  expect_s3_class(q, "projectData_catalog")
  expect_setequal(q$collection, c("main", "ref"))
  expect_equal(sort(q$binding[q$exposed_name %in% "cds_main"]), c("ambiguous", "ambiguous"))
  expect_output(print(q), "metadata only")
  expect_equal(nrow(data_collections()), 0)
  expect_equal(nrow(data_catalog()), 0)
  expect_identical(search(), sp)
  expect_identical(ls(globalenv(), all.names = TRUE), globals)
  expect_identical(options(), opts)
  expect_length(n$log, 0)
})

test_that("data_query() previews conflicts with what is already attached", {
  fx <- shared_fixtures()
  local_clean_session()
  data_attach("pdmain", lib.loc = fx$lib, id = "main")
  q <- data_query("pdref", lib.loc = fx$lib, id = "ref")
  expect_equal(unique(q$collection), "ref")
  expect_equal(q$binding[q$exposed_name %in% "cds_main"], "ambiguous")
  expect_equal(q$binding[q$exposed_name %in% "only_ref"], "bound")
  expect_equal(data_collections()$id, "main")
  expect_error(data_query(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("a", "b"),
                          suffix = c(".x", ".x")), class = "projectData_name_collision")
})

test_that("inventory, attachment, catalog printing and conflict handling open nothing", {
  fx <- shared_fixtures()
  local_clean_session()
  n <- local_open_counter()
  data_attach(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "ref"),
              suffix = c(NA, ".reference"))
  cat <- data_catalog()
  expect_output(print(cat), "metadata only")
  data_info("cds_main")
  data_prefer("cds_main", "main")
  data_primary("main")
  data_prefer("cds_main", NULL)
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

  data_attach(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "ref"))
  cat <- data_catalog()
  pair <- cat[cat$resource == "pair" & cat$collection == "main", ]
  expect_setequal(pair$object, c("pair_x", "pair_y"))
  multi <- cat[cat$resource == "multi" & cat$collection == "ref", ]
  expect_setequal(multi$object, c("m1", "m2"))
  expect_true(all(cat$mapping[cat$resource %in% c("pair", "multi")] == "recorded"))
  # original dataset identifiers are preserved and differ from object names
  expect_false("multi" %in% cat$object)
})

test_that("a multi-object resource is opened once however it is reached", {
  fx <- shared_fixtures()
  local_clean_session()
  n <- local_open_counter()
  data_attach("pdref", lib.loc = fx$lib, suffix = ".r")
  expect_identical(data_get("m1"), "m1")
  expect_identical(data_get("m1.r"), "m1")
  expect_identical(data_get("m2", "pdref"), "m2")
  expect_identical(get("m2", envir = as.environment("projectData:data")), "m2")
  expect_equal(n$log, "pdref/multi")
  cat <- data_catalog()
  expect_equal(unique(cat$state[cat$resource == "multi"]), "ready")
  expect_true(all(cat$mapping[cat$resource == "multi"] == "verified"))
})

test_that("the intended installation is selected regardless of later .libPaths()", {
  fx <- shared_fixtures()
  local_clean_session()
  data_attach("pdmain", lib.loc = fx$lib, id = "v1")
  expect_equal(data_collections()$version, "0.1.0")
  .libPaths(c(fx$lib2, .libPaths()))
  expect_identical(data_get("cds_main", "v1"), "main-v1")

  data_attach("pdmain", id = "v2")
  expect_equal(data_collections()$version[data_collections()$id == "v2"], "0.2.0")
  expect_identical(data_get("cds_main", "v2"), "main-v2")
  expect_error(data_attach("pdmain", lib.loc = file.path(tempdir(), "nowhere"), id = "z"),
               class = "projectData_not_installed")
})

test_that("incomplete mappings stay unknown until explicitly inspected", {
  fx <- shared_fixtures()
  local_clean_session()
  n <- local_open_counter()
  data_attach("pdref", lib.loc = fx$lib)
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
  cat <- data_query("datasets")
  b <- cat[cat$object %in% c("beaver1", "beaver2") & cat$kind == "original", ]
  expect_equal(unique(b$resource), "beavers")
  expect_length(n$log, 0)
})

test_that("global objects are never overwritten and shadow the facade with a warning", {
  fx <- shared_fixtures()
  local_clean_session()
  assign("only_ref", "global value", envir = globalenv())
  withr::defer(rm("only_ref", envir = globalenv()))
  expect_warning(data_attach("pdref", lib.loc = fx$lib), class = "projectData_masking")
  expect_identical(get("only_ref", envir = globalenv()), "global value")
  expect_identical(only_ref, "global value")
  expect_identical(data_get("only_ref"), 3)
  expect_true("only_ref" %in% data_masking()$name)
  expect_error(data_detach(), NA)
  expect_error(data_attach("pdref", lib.loc = fx$lib, on_mask = "error"), class = "projectData_masking")
  expect_false("projectData:data" %in% search())
})

test_that("the datalist index file is ignored silently; other unsupported files warn", {
  local_clean_session()
  lib <- fixture_library()
  root <- fixture_package(lib, "pddl", lazy = FALSE, datasets = list(a = list(a = 1)))
  writeLines("a: a", file.path(root, "data", "datalist"))
  expect_no_warning(data_query("pddl", lib.loc = lib))
  writeLines("x", file.path(root, "data", "notes.xyz"))
  expect_warning(data_query("pddl", lib.loc = lib), class = "projectData_unsupported_format")
})

test_that("an indexed dataset whose file is missing is cataloged with source NA", {
  lib <- fixture_library()
  fixture_package(lib, "pdgap", "0.1.0", lazy = FALSE, datasets = list(
    present = list(present_obj = 1), ghost = list(ghost_obj = 2)))
  unlink(file.path(lib, "pdgap", "data", "ghost.rda"))
  local_clean_session()
  n <- local_open_counter()

  data_attach("pdgap", lib.loc = lib, id = "gap")
  cat <- data_catalog()
  expect_equal(cat$source[cat$resource == "present"], "data/present.rda")
  ghost <- cat[cat$resource == "ghost", ]
  expect_true(is.na(ghost$source))
  expect_equal(ghost$object, "ghost_obj")
  expect_equal(ghost$state, "unopened")
  expect_length(n$log, 0)
})

test_that("a resource producing none of its recorded objects is failed and retryable", {
  lib <- fixture_library()
  fixture_package(lib, "pdgap", "0.1.0", lazy = FALSE, datasets = list(
    present = list(present_obj = 1), ghost = list(ghost_obj = 2)))
  ghost_file <- file.path(lib, "pdgap", "data", "ghost.rda")
  backup <- tempfile(fileext = ".rda")
  file.copy(ghost_file, backup)
  unlink(ghost_file)
  local_clean_session()
  data_attach("pdgap", lib.loc = lib, id = "gap")

  expect_error(suppressWarnings(data_get("ghost_obj")), "produced none",
               class = "projectData_opening_failed")
  cat <- data_catalog()
  expect_equal(cat$state[cat$resource == "ghost"], "failed")
  # failure is sticky and isolated from other resources
  expect_error(suppressWarnings(data_get("ghost_obj")), "data_retry",
               class = "projectData_opening_failed")
  expect_identical(data_get("present_obj"), 1)

  # restoring the file and retrying recovers the resource
  file.copy(backup, ghost_file)
  expect_equal(data_retry("gap", "ghost"), "ghost")
  expect_identical(data_get("ghost_obj"), 2)
  expect_equal(data_catalog()$state[data_catalog()$resource == "ghost"], "ready")
})
