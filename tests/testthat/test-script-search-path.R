# Fresh subprocesses keep the test runner's search path intact when standard
# packages are detached. The child loads this same copy of projectData.
run_child <- function(body, lib) {
  pkg <- system.file(package = "projectData")
  load_pkg <- if (dir.exists(file.path(pkg, "Meta"))) {
    sprintf("library(projectData, lib.loc = %s)", deparse(dirname(pkg)))
  } else {
    sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(pkg))
  }
  out <- tempfile("child-out-", fileext = ".rds")
  script <- tempfile("child-", fileext = ".R")
  withr::defer(unlink(c(out, script)))
  writeLines(c(
    "options(warn = 1)",
    load_pkg,
    sprintf("lib <- %s", deparse(lib)),
    sprintf("out <- %s", deparse(out)),
    "counter <- new.env(); counter$n <- 0L",
    "data_config(messages = FALSE, on_open = function(info) counter$n <- counter$n + 1L)",
    "options(pdsp_nested = TRUE)",
    "sp_pos <- function(p) match(p, search())",
    body,
    "saveRDS(res, out)"
  ), script)
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(log, "status") %||% 0L
  info <- paste(c(sprintf("child exit status %s", status), log), collapse = "\n")
  if (!identical(status, 0L) || !file.exists(out)) {
    testthat::fail(paste("child process failed:", info))
    return(NULL)
  }
  res <- readRDS(out)
  res$log <- info
  res
}

search_path_library <- function(env = parent.frame()) {
  lib <- fixture_library(env)
  inner <- sprintf("projectData::data_get('inner_val', package = 'pdinner', lib.loc = %s)",
                   deparse(lib))
  # A separate collection: opening a resource of the collection being opened would re-register it
  fixture_package(lib, "pdinner", "0.1.0", lazy = FALSE,
    scripts = list(inner = "inner_val <- head(LETTERS, 2)"),
    script_docs = list(inner = "inner_val"))
  fixture_package(lib, "pdsp", "0.1.0", lazy = FALSE,
    datasets = list(ser = list(ser_obj = "serialized")),
    scripts = list(
      plain = "plain_obj <- head(letters, 3)",
      boom = c("boom_pre <- 1", "stop('script failure')"),
      rel = c("rel_val <- readLines('../extdata/in.txt')",
              "rel_wd <- basename(getwd())"),
      relfail = c("relfail_val <- readLines('../extdata/in.txt')", "stop('after read')"),
      # Guarded: R CMD INSTALL evaluates scripts, outside the child process
      nest = paste0("nest_val <- if (isTRUE(getOption('pdsp_nested'))) ", inner, " else NA")
    ),
    script_docs = list(plain = "plain_obj", boom = "boom_pre",
                       rel = c("rel_val", "rel_wd"), relfail = "relfail_val",
                       nest = "nest_val"),
    files = list("extdata/in.txt" = function(path) writeLines("from-extdata", path)))
  lib
}

test_that("script retrieval with utils detached returns the object and leaves the search path unchanged", {
  lib <- search_path_library()
  res <- run_child(c(
    "detach('package:utils')",
    "before <- search()",
    "val <- data_get('plain_obj', package = 'pdsp', lib.loc = lib)",
    "res <- list(val = val, before = before, after = search())"
  ), lib)
  expect_identical(res$val, c("a", "b", "c"), info = res$log)
  expect_false("package:utils" %in% res$before)
  expect_identical(res$after, res$before, info = res$log)
})

test_that("a failing script leaves no implicit utils attachment", {
  lib <- search_path_library()
  res <- run_child(c(
    "detach('package:utils')",
    "before <- search()",
    "err <- tryCatch(data_get('boom_pre', package = 'pdsp', lib.loc = lib), error = function(e) conditionMessage(e))",
    "state <- data_catalog()$state[data_catalog()$object %in% 'boom_pre']",
    "res <- list(err = err, state = state, before = before, after = search())"
  ), lib)
  expect_match(res$err, "script failure", info = res$log)
  expect_identical(res$state, "failed", info = res$log)
  expect_identical(res$after, res$before, info = res$log)
})

test_that("a preexisting utils attachment keeps its environment and position", {
  lib <- search_path_library()
  res <- run_child(c(
    "env_before <- as.environment('package:utils'); pos_before <- sp_pos('package:utils')",
    "before <- search()",
    "ok <- data_get('plain_obj', package = 'pdsp', lib.loc = lib)",
    "err <- tryCatch(data_get('boom_pre', package = 'pdsp', lib.loc = lib), error = function(e) 'err')",
    "res <- list(ok = ok, err = err, before = before, after = search(),",
    "  same_env = identical(env_before, as.environment('package:utils')),",
    "  pos_before = pos_before, pos_after = sp_pos('package:utils'))"
  ), lib)
  expect_identical(res$ok, c("a", "b", "c"), info = res$log)
  expect_identical(res$err, "err")
  expect_identical(res$after, res$before, info = res$log)
  expect_true(res$same_env)
  expect_identical(res$pos_after, res$pos_before)
})

test_that("unqualified standard functions work through the lookup environment when utils is detached", {
  lib <- search_path_library()
  # head() lives in utils, which is absent from the search path here
  res <- run_child(c(
    "detach('package:utils')",
    "res <- list(head_on_path = exists('head', mode = 'function'),",
    "  val = data_get('plain_obj', package = 'pdsp', lib.loc = lib))"
  ), lib)
  expect_false(res$head_on_path, info = res$log)
  expect_identical(res$val, c("a", "b", "c"), info = res$log)
})

test_that("nested resource opening neither leaks nor removes a utils attachment", {
  lib <- search_path_library()
  detached <- run_child(c(
    "detach('package:utils')",
    "before <- search()",
    "val <- data_get('nest_val', package = 'pdsp', lib.loc = lib)",
    "res <- list(val = val, before = before, after = search(),",
    "  inner_ready = 'ready' %in% data_catalog()$state[data_catalog()$object %in% 'inner_val'])"
  ), lib)
  expect_identical(detached$val, c("A", "B"), info = detached$log)
  expect_true(detached$inner_ready, info = detached$log)
  expect_identical(detached$after, detached$before, info = detached$log)

  attached <- run_child(c(
    "env_before <- as.environment('package:utils'); pos_before <- sp_pos('package:utils')",
    "before <- search()",
    "val <- data_get('nest_val', package = 'pdsp', lib.loc = lib)",
    "res <- list(val = val, before = before, after = search(),",
    "  same_env = identical(env_before, as.environment('package:utils')),",
    "  same_pos = identical(pos_before, sp_pos('package:utils')))"
  ), lib)
  expect_identical(attached$val, c("A", "B"), info = attached$log)
  expect_identical(attached$after, attached$before, info = attached$log)
  expect_true(attached$same_env && attached$same_pos, info = attached$log)
})

test_that("script-relative file access works and the working directory is restored", {
  lib <- search_path_library()
  res <- run_child(c(
    "detach('package:utils')",
    "wd <- getwd(); before <- search()",
    "val <- data_get('rel_val', package = 'pdsp', lib.loc = lib)",
    "wd_ok <- getwd(); dir_ok <- data_get('rel_wd', package = 'pdsp', lib.loc = lib)",
    "err <- tryCatch(data_get('relfail_val', package = 'pdsp', lib.loc = lib), error = function(e) conditionMessage(e))",
    "res <- list(val = val, dir_ok = dir_ok, err = err, wd = wd, wd_ok = wd_ok,",
    "  wd_after = getwd(), before = before, after = search())"
  ), lib)
  expect_identical(res$val, "from-extdata", info = res$log)
  expect_identical(res$dir_ok, "data", info = res$log)
  expect_match(res$err, "after read", info = res$log)
  expect_identical(res$wd_ok, res$wd)
  expect_identical(res$wd_after, res$wd)
  expect_identical(res$after, res$before, info = res$log)
})

test_that("repeated access uses the cache and does not touch the search path", {
  lib <- search_path_library()
  res <- run_child(c(
    "detach('package:utils')",
    "before <- search()",
    "a <- data_get('plain_obj', package = 'pdsp', lib.loc = lib)",
    "n1 <- counter$n",
    "b <- data_get('plain_obj', package = 'pdsp', lib.loc = lib)",
    "res <- list(same = identical(a, b), n1 = n1, n2 = counter$n, before = before, after = search())"
  ), lib)
  expect_true(res$same, info = res$log)
  expect_identical(c(res$n1, res$n2), c(1L, 1L), info = res$log)
  expect_identical(res$after, res$before, info = res$log)
})

test_that("serialized datasets still load with utils detached", {
  lib <- search_path_library()
  res <- run_child(c(
    "detach('package:utils')",
    "before <- search()",
    "res <- list(val = data_get('ser_obj', package = 'pdsp', lib.loc = lib), before = before, after = search())"
  ), lib)
  expect_identical(res$val, "serialized", info = res$log)
  expect_identical(res$after, res$before, info = res$log)
})
