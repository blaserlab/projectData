logging_loader <- function(id, log, validate = NULL) {
  data_loader(id,
    validate = validate %||% function(descriptor, root, context) {
      log$events[[length(log$events) + 1L]] <- list(what = "validate", id = id, context = context,
                                                   root = root, descriptor = descriptor)
      TRUE
    },
    open = function(descriptor, paths, context) {
      log$events[[length(log$events) + 1L]] <- list(what = "open", id = id, context = context,
                                                   paths = paths)
      stats::setNames(list(id), descriptor$objects)
    })
}

new_log <- function() {
  log <- new.env()
  log$events <- list()
  log$what <- function() vapply(log$events, function(e) e$what, "")
  log
}

test_that("validators receive the documented context and resolve paths in the right package", {
  local_clean_session()
  fx <- shared_fixtures()
  log <- new_log()
  data_loader_add(logging_loader("test::flaky", log))
  data_loader_add(logging_loader("test::reenter", log))
  data_attach("pdmisc", lib.loc = fx$lib, id = "misc")
  expect_identical(data_get("flaky_obj"), "test::flaky")

  v <- Filter(function(e) e$what == "validate" && e$id == "test::flaky", log$events)
  expect_length(v, 2L)
  for (e in v) {
    expect_named(e$context, c("collection", "package", "version", "library", "root", "resource"))
    expect_equal(e$context$collection, "misc")
    expect_equal(e$context$package, "pdmisc")
    expect_equal(e$context$version, "0.1.0")
    expect_equal(e$context$resource, "flaky")
    expect_equal(e$context$library, normalizePath(fx$lib, winslash = "/"))
    expect_identical(e$root, e$context$root)
    expect_true(file.exists(file.path(e$root, e$descriptor$source)))
    expect_equal(basename(e$root), "pdmisc")
  }
  opened <- Filter(function(e) e$what == "open", log$events)[[1]]
  shared <- setdiff(names(v[[2]]$context), character())
  expect_identical(opened$context[shared], v[[2]]$context[shared])
})

test_that("a collection registered without its loader is validated once the loader appears", {
  local_clean_session()
  fx <- shared_fixtures()
  log <- new_log()
  data_attach("pdmisc", lib.loc = fx$lib)
  expect_error(data_get("flaky_obj"), class = "projectData_loader_unavailable")
  expect_length(log$events, 0)

  data_loader_add(logging_loader("test::flaky", log))
  data_loader_add(logging_loader("test::reenter", log))
  expect_identical(data_get("flaky_obj"), "test::flaky")
  expect_equal(log$what(), c("validate", "open"))
  # cached: neither validated nor opened again
  data_get("flaky_obj")
  expect_equal(log$what(), c("validate", "open"))
})

test_that("automatic adapter resolution validates before opening", {
  local_clean_session()
  lib <- validation_fixtures()
  local_adapter_lib(lib)
  log <- local_event_log()
  data_attach("pdvalpkg", lib.loc = lib)
  expect_false("pdvaladp::v" %in% data_loaders()$id)
  expect_length(log$events, 0)

  expect_identical(data_get("vres_obj"), "opened")
  expect_true("pdvaladp::v" %in% data_loaders()$id)
  expect_equal(log$what(), c("validate", "open"))
  v <- log$events[[1]]
  expect_equal(v$context$package, "pdvalpkg")
  expect_equal(v$context$version, "0.3.0")
  expect_equal(v$id, "pdvaladp::v")
})

test_that("a validation failure blocks the reader, is explained, and can be retried", {
  local_clean_session()
  lib <- validation_fixtures()
  local_adapter_lib(lib)
  log <- local_event_log()
  data_attach("pdvalpkg", lib.loc = lib)

  options(pdtest.reject = TRUE)
  expect_error(data_get("vres_obj"), "validation by loader 'pdvaladp::v' failed: rejected by test",
               class = "projectData_validation_failed")
  expect_equal(log$what(), "validate")
  cat <- data_catalog()
  expect_equal(cat$state[cat$object %in% "vres_obj"], "failed")
  # failed state is sticky until retried, and still does not open
  expect_error(data_get("vres_obj"), class = "projectData_opening_failed")
  expect_equal(log$what(), "validate")

  options(pdtest.reject = NULL)
  data_retry("pdvalpkg", "vres")
  expect_identical(data_get("vres_obj"), "opened")
  expect_equal(log$what(), c("validate", "validate", "open"))
})

test_that("a validator that errors is reported and does not leave an opening state", {
  local_clean_session()
  fx <- shared_fixtures()
  log <- new_log()
  data_loader_add(logging_loader("test::reenter", log))
  data_loader_add(logging_loader("test::flaky", log))
  data_attach("pdmisc", lib.loc = fx$lib)
  data_loader_add(logging_loader("test::flaky", log,
                                 validate = function(descriptor, root, context) stop("boom")),
                  replace = TRUE)
  expect_error(data_get("flaky_obj"), "validator failed: boom")
  cat <- data_catalog()
  expect_equal(cat$state[cat$object %in% "flaky_obj"], "failed")
  expect_false("open" %in% log$what())
})

test_that("a replacement loader is validated by its own validator", {
  local_clean_session()
  fx <- shared_fixtures()
  log <- new_log()
  data_loader_add(logging_loader("test::flaky", log))
  data_loader_add(logging_loader("test::reenter", log))
  data_attach("pdmisc", lib.loc = fx$lib)

  reject <- function(descriptor, root, context) "replacement says no"
  data_loader_add(logging_loader("test::flaky", log, validate = reject), replace = TRUE)
  expect_error(data_get("flaky_obj"), "replacement says no")
  expect_false("open" %in% log$what())

  data_loader_add(logging_loader("test::flaky", log), replace = TRUE)
  data_retry("pdmisc", "flaky")
  expect_identical(data_get("flaky_obj"), "test::flaky")
  expect_equal(tail(log$what(), 2), c("validate", "open"))
})

test_that("a multi-loader adapter registers completely", {
  local_clean_session()
  lib <- validation_fixtures()
  local_adapter_lib(lib)
  log <- local_event_log()
  expect_equal(data_loader_adapter("pdvaladp"), c("pdvaladp::v", "pdvaladp::w"))
  expect_true(all(c("pdvaladp::v", "pdvaladp::w") %in% data_loaders()$id))
  expect_length(log$events, 0)
})

test_that("a later collision leaves the registry unchanged", {
  local_clean_session()
  lib <- validation_fixtures()
  local_adapter_lib(lib)
  log <- local_event_log()
  data_loader_add(data_loader("pdvaladp::w", open = function(descriptor, paths, context) list(x = 1)))
  before <- projectData:::.state$loaders
  expect_error(data_loader_adapter("pdvaladp", "loaders_collide"), "'pdvaladp::w' is already registered.*Nothing was registered")
  expect_identical(projectData:::.state$loaders, before)
  expect_false(any(c("pdvaladp::fresh", "pdvaladp::v") %in% data_loaders()$id))
  expect_length(log$events, 0)

  # with replace = TRUE the same declaration is accepted whole
  data_loader_adapter("pdvaladp", "loaders_collide", replace = TRUE)
  expect_true(all(c("pdvaladp::fresh", "pdvaladp::v", "pdvaladp::w") %in% data_loaders()$id))
})

test_that("malformed declarations and duplicate ids register nothing", {
  local_clean_session()
  lib <- validation_fixtures()
  local_adapter_lib(lib)
  log <- local_event_log()
  before <- projectData:::.state$loaders
  expect_error(data_loader_adapter("pdvaladp", "loaders_dup"), "more than once.*'pdvaladp::a'")
  expect_error(data_loader_adapter("pdvaladp", "loaders_malformed"), "`open` must be a function")
  expect_error(data_loader_adapter("pdvaladp", "loaders_nonloader"), "Expected a loader")
  expect_error(data_loader_adapter("pdvaladp", "loaders_none"), "no loaders")
  expect_identical(projectData:::.state$loaders, before)
  expect_length(log$events, 0)

  bad <- data_loader("test::x", open = identity)
  bad$needs_work_dir <- NA
  expect_error(data_loader_add(bad), "needs_work_dir")
  bad <- data_loader("test::x", open = identity); bad$storage <- "weird"
  expect_error(data_loader_add(bad), "storage")
  expect_identical(projectData:::.state$loaders, before)
})

test_that("metadata operations still never open resources and caching is preserved", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_attach("pdmain", lib.loc = fx$lib)
  data_catalog()
  expect_length(n$log, 0)
  data_get("meta_a"); data_get("meta_b")
  expect_equal(n$log, "pdmain/meta")
})
