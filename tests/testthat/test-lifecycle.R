test_that("attach, add, detach lifecycle cleans up only owned state", {
  local_clean_session()
  fx <- shared_fixtures()
  before_search <- search()
  data_attach("pdmain", lib.loc = fx$lib, id = "main", primary = "main")
  expect_identical(search()[2], "projectData:data")
  data_attach("pdref", lib.loc = fx$lib, id = "ref", suffix = ".ref")
  expect_true(all(c("meta_a", "only_ref", "only_ref.ref") %in% ls(as.environment("projectData:data"))))

  obj <- get("meta_a")
  data_detach("ref")
  expect_false("only_ref" %in% ls(as.environment("projectData:data")))
  expect_true("meta_a" %in% ls(as.environment("projectData:data")))
  expect_equal(data_collections()$id, "main")
  data_detach("main")
  expect_identical(obj, 1)
  expect_equal(nrow(data_collections()), 0)
  expect_true(is.na(data_primary()))
  expect_error(data_get("meta_a"), class = "projectData_not_found")
  expect_identical(search(), before_search)
})

test_that("detaching forgets preferences and the primary of that collection", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "ref"), primary = "ref")
  data_prefer("cds_main", "main")
  data_detach("main")
  expect_equal(nrow(data_preferences()), 0)
  expect_equal(data_primary(), "ref")
  data_detach("ref")
  expect_true(is.na(data_primary()))
})

test_that("loading projectData and querying leave options, search path and globals alone; attaching leaves options and globals alone", {
  local_clean_session()
  fx <- shared_fixtures()
  opts <- options()
  globals <- ls(globalenv(), all.names = TRUE)
  sp <- search()
  data_query("pdmain", lib.loc = fx$lib)
  expect_identical(options(), opts)
  expect_identical(ls(globalenv(), all.names = TRUE), globals)
  expect_identical(search(), sp)
  data_attach("pdmain", lib.loc = fx$lib)
  expect_identical(options(), opts)
  expect_identical(ls(globalenv(), all.names = TRUE), globals)
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
  data_attach("pdmisc", lib.loc = fx$lib)
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
  data_attach("pdswap", lib.loc = lib_b)
  unlink(file.path(lib_b, "pdswap"), recursive = TRUE)
  fixture_package(lib_b, "pdswap", "0.1.0", datasets = list(a = list(a = "rebuilt")))
  expect_error(data_get("a"), class = "projectData_stale_installation")
  expect_equal(data_catalog()$state, "unopened")
  data_attach("pdswap", lib.loc = lib_b, replace = TRUE)
  expect_identical(data_get("a"), "rebuilt")
})

test_that("data_attach() is idempotent per id and refuses mismatched ids", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_attach(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "ref"), primary = "main")
  expect_equal(data_collections()$id, c("main", "ref"))
  expect_length(n$log, 0)
  expect_identical(cds_main, "main-v1")
  expect_identical(only_ref, 3)
  # same id, same package: nothing changes
  data_attach("pdmain", id = "main")
  expect_equal(data_collections()$id, c("main", "ref"))
  # same id, different package: refused
  expect_error(data_attach("pdref", id = "main"), "already attached from package")
  # registration arguments for an attached id need replace = TRUE
  expect_error(data_attach("pdref", id = "ref", suffix = ".r", lib.loc = fx$lib), "already attached")
  data_attach("pdref", id = "ref", suffix = ".r", replace = TRUE, lib.loc = fx$lib)
  expect_identical(only_ref.r, 3)
})

test_that("a failed data_attach() attaches nothing", {
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

test_that("re-attaching an attached collection whose installation changed is refused", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdchg", "0.1.0", datasets = list(a = list(a = "old")))
  data_attach("pdchg", lib.loc = lib)
  unlink(file.path(lib, "pdchg"), recursive = TRUE)
  fixture_package(lib, "pdchg", "0.1.0", datasets = list(a = list(a = "new")))
  expect_error(data_attach("pdchg"), class = "projectData_stale_installation")
  data_attach("pdchg", lib.loc = lib, replace = TRUE)
  expect_identical(a, "new")
})

test_that("the first-access message names objects and size, and can be turned off", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib)
  expect_equal(data_config()$messages, FALSE) # set by local_clean_session()
  data_config(messages = TRUE)
  expect_message(data_get("m1"), "object\\(s\\) m1, m2 \\[.* on disk: data/multi.rda\\]")
  expect_message(data_get("only_ref"), "nothing is downloaded")
  expect_no_message(data_get("only_ref"))
  data_config(messages = FALSE)
  expect_no_message(data_get("ref_rds"))
})

test_that("on_open runs under the opening guard: self access is a reentrancy error", {
  local_clean_session()
  fx <- shared_fixtures()
  reads <- 0
  data_loader_add(data_loader("test::flaky", open = function(descriptor, paths, context) {
    reads <<- reads + 1
    list(flaky_obj = "ok")
  }))
  data_loader_add(data_loader("test::reenter", open = function(descriptor, paths, context) {
    list(reenter_obj = "ok")
  }))
  data_attach("pdmisc", lib.loc = fx$lib)
  callbacks <- 0
  data_config(on_open = function(info) {
    callbacks <<- callbacks + 1
    data_get("flaky_obj")
  })
  expect_error(data_get("flaky_obj"), "already being opened", class = "projectData_opening_failed")
  expect_equal(c(callbacks, reads), c(1, 0))
  expect_equal(data_catalog()$state[data_catalog()$resource == "flaky"], "failed")

  # Failure is recorded through the usual mechanism and retry is explicit.
  expect_error(data_get("flaky_obj"), "data_retry", class = "projectData_opening_failed")
  expect_equal(callbacks, 1)
  data_config(on_open = NULL)
  data_retry("pdmisc", "flaky")
  expect_identical(data_get("flaky_obj"), "ok")
  expect_equal(reads, 1)
})

test_that("on_open reaching the same resource via an alias or sibling hits the same guard", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib, id = "ref", suffix = ".ref")
  callbacks <- character()
  data_config(on_open = function(info) {
    callbacks <<- c(callbacks, info$resource)
    if (info$resource == "multi") data_get(getOption("pdtest.target"))
  })
  for (target in c("m2", "m1.ref")) {
    withr::local_options(pdtest.target = target)
    callbacks <- character()
    expect_error(data_get("m1"), "already being opened", class = "projectData_opening_failed")
    expect_equal(callbacks, "multi")
    data_config(on_open = NULL)
    data_retry("ref", "multi")
    data_config(on_open = function(info) {
      callbacks <<- c(callbacks, info$resource)
      if (info$resource == "multi") data_get(getOption("pdtest.target"))
    })
  }
  data_config(on_open = NULL)
  data_retry("ref", "multi")
  expect_identical(data_get("m2"), "m2")
})

test_that("an on_open error is an opening failure and retry works once corrected", {
  local_clean_session()
  fx <- shared_fixtures()
  reads <- 0
  data_loader_add(data_loader("test::flaky", open = function(descriptor, paths, context) {
    reads <<- reads + 1
    list(flaky_obj = "ok")
  }))
  data_loader_add(data_loader("test::reenter", open = function(descriptor, paths, context) list(reenter_obj = 1)))
  data_attach("pdmisc", lib.loc = fx$lib)
  data_config(on_open = function(info) stop("audit log unavailable"))
  expect_error(data_get("flaky_obj"), "audit log unavailable", class = "projectData_opening_failed")
  expect_equal(reads, 0)
  expect_equal(data_catalog()$state[data_catalog()$resource == "flaky"], "failed")
  counter <- local_open_counter()
  data_retry("pdmisc", "flaky")
  expect_identical(data_get("flaky_obj"), "ok")
  expect_equal(c(reads, length(counter$log)), c(1, 1))
})

test_that("on_open may open an independent resource; cached access reruns nothing", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib, id = "ref")
  log <- character()
  data_config(on_open = function(info) {
    log <<- c(log, info$resource)
    if (info$resource == "multi") data_get("only_ref")
  })
  expect_identical(data_get("m1"), "m1")
  expect_equal(log, c("multi", "only"))
  expect_identical(data_get("m2"), "m2")
  expect_identical(data_get("m1"), "m1")
  expect_identical(data_get("only_ref"), 3)
  expect_equal(log, c("multi", "only"))
  expect_equal(unique(data_catalog()$state[data_catalog()$resource %in% c("multi", "only")]), "ready")
})
