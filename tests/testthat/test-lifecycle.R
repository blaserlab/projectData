test_that("add, attach, detach, unregister lifecycle cleans up only owned state", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  before_search <- search()
  data_register("pdmain", lib.loc = fx$lib, id = "main", primary = "main")
  expect_identical(search(), before_search)
  data_attach()
  expect_identical(search()[2], "projectData:data")
  data_register("pdref", lib.loc = fx$lib, id = "ref", suffix = ".ref")
  # registering later does not silently attach the new collection
  expect_false("only_ref" %in% ls(as.environment("projectData:data")))
  data_attach("ref")
  expect_true("only_ref" %in% ls(as.environment("projectData:data")))

  obj <- get("meta_a")
  data_detach("ref")
  expect_false("only_ref" %in% ls(as.environment("projectData:data")))
  data_unregister("main")
  expect_identical(obj, 1)
  expect_equal(data_collections()$id, "ref")
  expect_equal(is.na(data_primary()), TRUE)
  expect_error(data_get("meta_a"), class = "projectData_not_found")
  data_detach()
  expect_identical(search(), before_search)
})

test_that("loading projectData and registering does not touch options, search path or globals", {
  local_clean_session()
  fx <- shared_fixtures()
  opts <- options()
  globals <- ls(globalenv(), all.names = TRUE)
  sp <- search()
  data_register("pdmain", lib.loc = fx$lib)
  data_catalog()
  expect_identical(options(), opts)
  expect_identical(ls(globalenv(), all.names = TRUE), globals)
  expect_identical(search(), sp)
})

test_that("failure is isolated, reported, and retry is explicit", {
  local_clean_session()
  fx <- shared_fixtures()
  calls <- 0
  data_loader_add(data_loader("test::flaky", open = function(descriptor, paths, context) {
    calls <<- calls + 1
    if (calls == 1) stop("backing file temporarily unavailable")
    list(flaky_obj = "recovered")
  }))
  data_loader_add(data_loader("test::reenter", open = function(descriptor, paths, context) {
    list(reenter_obj = data_get("reenter_obj"))
  }))
  data_register("pdmisc", lib.loc = fx$lib)
  expect_error(data_get("flaky_obj"), "temporarily unavailable", class = "projectData_opening_failed")
  cat <- data_catalog()
  expect_equal(cat$state[cat$resource == "flaky"], "failed")
  expect_equal(calls, 1)
  # failure is sticky until an explicit retry, and does not affect other resources
  expect_error(data_get("flaky_obj"), "data_retry", class = "projectData_opening_failed")
  expect_equal(calls, 1)
  expect_identical(data_get("plain_obj"), 1)
  expect_equal(data_retry("pdmisc"), "flaky")
  expect_identical(data_get("flaky_obj"), "recovered")
  expect_identical(data_get("flaky_obj"), "recovered")
  expect_equal(calls, 2)

  expect_error(data_get("reenter_obj"), "reentrant|already being opened")
  expect_equal(data_catalog()$state[data_catalog()$resource == "reenter"], "failed")
})

test_that("replacing an installation in place is detected before opening", {
  local_clean_session()
  lib_b <- fixture_library()
  fixture_package(lib_b, "pdswap", "0.1.0", datasets = list(a = list(a = "old")))
  data_register("pdswap", lib.loc = lib_b)
  unlink(file.path(lib_b, "pdswap"), recursive = TRUE)
  fixture_package(lib_b, "pdswap", "0.1.0", datasets = list(a = list(a = "rebuilt")))
  expect_error(data_get("a"), class = "projectData_stale_installation")
  expect_equal(data_catalog()$state, "unopened")
  data_register("pdswap", lib.loc = lib_b, replace = TRUE)
  expect_identical(data_get("a"), "rebuilt")
})

test_that("data_attach() registers package names and attaches in one step", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  expect_false("projectData:data" %in% search())
  data_attach(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "ref"), primary = "main")
  expect_equal(data_collections()$id, c("main", "ref"))
  expect_true(all(data_collections()$attached))
  expect_length(n$log, 0)
  expect_identical(cds_main, "main-v1")
  expect_identical(only_ref, 3)
  # attaching an id that is already registered just attaches it
  data_detach()
  expect_false(any(data_collections()$attached))
  data_attach("ref")
  expect_equal(data_collections()$attached, c(FALSE, TRUE))
  expect_error(data_attach("ref", suffix = ".r"), "already registered")
  data_attach("pdref", id = "ref", suffix = ".r", replace = TRUE, lib.loc = fx$lib)
  expect_identical(only_ref.r, 3)
})

test_that("a failed data_attach() registers and attaches nothing", {
  local_clean_session()
  fx <- shared_fixtures()
  expect_error(
    data_attach(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("a", "b"), suffix = c(".x", ".x")),
    class = "projectData_name_collision"
  )
  expect_equal(nrow(data_collections()), 0)
  expect_false("projectData:data" %in% search())
  expect_error(data_attach("nonexistentpkg"), class = "projectData_not_installed")
  expect_equal(nrow(data_collections()), 0)
})

test_that("attaching a registered collection whose installation changed is refused", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdchg", "0.1.0", datasets = list(a = list(a = "old")))
  data_register("pdchg", lib.loc = lib)
  unlink(file.path(lib, "pdchg"), recursive = TRUE)
  fixture_package(lib, "pdchg", "0.1.0", datasets = list(a = list(a = "new")))
  expect_error(data_attach("pdchg"), class = "projectData_stale_installation")
  expect_false("projectData:data" %in% search())
  data_attach("pdchg", lib.loc = lib, replace = TRUE)
  expect_identical(a, "new")
})
