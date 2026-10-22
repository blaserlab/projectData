attached_names <- function() {
  if (!"projectData:data" %in% search()) return(character())
  sort(ls(as.environment("projectData:data")))
}

test_that("data_get(package =) registers on demand and leaves the search path alone", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  sp <- search()
  globals <- ls(globalenv(), all.names = TRUE)

  expect_identical(data_get("cds_main", package = "pdmain", lib.loc = fx$lib), "main-v1")
  expect_identical(search(), sp)
  expect_identical(ls(globalenv(), all.names = TRUE), globals)
  expect_identical(n$log, "pdmain/cds_main")

  co <- data_collections()
  expect_equal(co$id, "pdmain")
  expect_false(co$attached)
  # Other resources of the collection are registered but unopened.
  cat <- data_catalog()
  expect_equal(cat$state[cat$resource == "cds_main"], "ready")
  expect_true(all(cat$state[cat$resource != "cds_main"] == "unopened"))
  expect_true(all(cat$binding == "unattached"))
  expect_error(data_get("cds_main"), class = "projectData_not_found")
})

test_that("a sibling-object resource opens once for all of its objects", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_get("meta_a", package = "pdmain", lib.loc = fx$lib)
  expect_identical(data_get("meta_b", package = "pdmain"), 2)
  expect_identical(n$log, "pdmain/meta")
})

test_that("data_attach() after data_get() reuses the registration and the cache", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_get("cds_main", package = "pdmain", lib.loc = fx$lib)
  data_attach("pdmain")
  expect_equal(nrow(data_collections()), 1L)
  expect_true(data_collections()$attached)
  expect_identical(cds_main, "main-v1")
  expect_identical(n$log, "pdmain/cds_main")
  expect_identical(meta_a, 1)
  expect_identical(n$log, c("pdmain/cds_main", "pdmain/meta"))
})

test_that("attaching an unattached registration with new arguments keeps cached resources", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_get("cds_main", package = "pdmain", lib.loc = fx$lib)
  data_attach("pdmain", lib.loc = fx$lib, suffix = ".m")
  expect_equal(nrow(data_collections()), 1L)
  expect_identical(cds_main.m, "main-v1")
  expect_identical(n$log, "pdmain/cds_main")
})

test_that("attached access followed by data_get() shares the cache and the attachment", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_attach("pdmain", lib.loc = fx$lib, suffix = ".m")
  expect_identical(cds_main.m, "main-v1")
  sp <- search()
  bound <- attached_names()
  expect_identical(data_get("cds_main", package = "pdmain"), "main-v1")
  expect_identical(data_get("cds_main", package = "pdmain", lib.loc = fx$lib), "main-v1")
  expect_identical(n$log, "pdmain/cds_main")
  expect_identical(search(), sp)
  expect_identical(attached_names(), bound)
  expect_true(data_collections()$attached)
  expect_equal(nrow(data_collections()), 1L)
})

test_that("package-qualified retrieval ignores preferences, primary and suffixes", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach(c("pdmain", "pdref"), lib.loc = fx$lib, id = c("main", "ref"),
              suffix = c(NA, ".ref"), primary = "main")
  data_prefer("cds_main", "main")
  expect_identical(cds_main, "main-v1")
  expect_identical(data_get("cds_main"), "main-v1")
  expect_identical(data_get("cds_main", package = "pdref"), "ref")
  expect_identical(data_get("cds_main", package = "pdmain"), "main-v1")
  expect_identical(data_get("cds_main.ref"), "ref")
  # Aliases are not original names.
  expect_error(data_get("cds_main.ref", package = "pdref"), "does not expose")
  expect_identical(data_get(c("cds_main", "cds_main"), package = c("pdref", "pdmain")),
                   list(cds_main = "ref", cds_main = "main-v1"))
  # Attached collections were reused, not duplicated.
  expect_equal(data_collections()$id, c("main", "ref"))
})

test_that("unqualified ambiguity errors are unchanged", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach(c("pdmain", "pdref"), lib.loc = fx$lib)
  expect_error(data_get("cds_main"), class = "projectData_ambiguous")
  expect_identical(data_get("cds_main", package = "pdref"), "ref")
  expect_identical(data_get("cds_main", collection = "pdmain"), "main-v1")
  # An unattached registration takes no part in bare-name resolution.
  data_detach("pdmain")
  expect_identical(data_get("cds_main"), "ref")
  data_get("cds_main", package = "pdmain", lib.loc = fx$lib)
  expect_false(data_collections()$attached[data_collections()$id == "pdmain"])
  expect_identical(data_get("cds_main"), "ref")
  expect_true(all(attached_names() != "meta_a"))
})

test_that("repeated operations never duplicate records or search-path entries", {
  local_clean_session()
  fx <- shared_fixtures()
  for (i in 1:3) {
    data_get("cds_main", package = "pdmain", lib.loc = fx$lib)
    data_get("only_ref", package = "pdref", lib.loc = fx$lib)
  }
  expect_equal(sort(data_collections()$id), c("pdmain", "pdref"))
  expect_false("projectData:data" %in% search())
  for (i in 1:3) data_attach("pdmain")
  data_get("cds_main", package = "pdmain")
  expect_equal(sort(data_collections()$id), c("pdmain", "pdref"))
  expect_equal(sum(search() == "projectData:data"), 1L)
  expect_equal(data_collections()$attached[order(data_collections()$id)], c(TRUE, FALSE))
})

test_that("installation selection and id checks protect explicit retrieval", {
  local_clean_session()
  fx <- shared_fixtures()
  data_get("cds_main", package = "pdmain", lib.loc = fx$lib)
  # The registered copy stays in force; a different library is not chosen silently.
  expect_error(data_get("cds_main", package = "pdmain", lib.loc = fx$lib2), "another installation")
  expect_identical(data_get("cds_main", package = "pdmain", collection = "v2", lib.loc = fx$lib2),
                   "main-v2")
  expect_identical(data_get("cds_main", collection = "pdmain"), "main-v1")
  expect_setequal(data_collections()$id, c("pdmain", "v2"))
  # Both come from the package: bare package is ambiguous unless lib.loc picks one.
  expect_error(data_get("cds_main", package = "pdmain"), class = "projectData_ambiguous")
  expect_identical(data_get("cds_main", package = "pdmain", lib.loc = fx$lib2), "main-v2")
  # An id cannot silently point at another package.
  expect_error(data_get("only_ref", package = "pdref", collection = "pdmain", lib.loc = fx$lib),
               "already registered from package")
  # Failed registration leaves nothing behind.
  expect_error(data_get("x", package = "nonexistentpkg"), class = "projectData_not_installed")
  expect_setequal(data_collections()$id, c("pdmain", "v2"))
})

test_that("a stale installation is detected for explicit and attached access", {
  local_clean_session()
  lib <- fixture_library()
  n <- local_open_counter()
  fixture_package(lib, "pdstale", "0.1.0", lazy = FALSE, datasets = list(a = list(a = "old"), b = list(b = "x")))
  expect_identical(data_get("a", package = "pdstale", lib.loc = lib), "old")
  unlink(file.path(lib, "pdstale"), recursive = TRUE)
  fixture_package(lib, "pdstale", "0.1.0", lazy = FALSE, datasets = list(a = list(a = "new"), b = list(b = "rebuilt")))
  expect_error(data_get("b", package = "pdstale"), class = "projectData_stale_installation")
  expect_error(data_attach("pdstale"), class = "projectData_stale_installation")
  expect_false(data_collections()$attached)
  expect_identical(n$log, "pdstale/a")
  data_attach("pdstale", lib.loc = lib, replace = TRUE)
  expect_identical(b, "rebuilt")
})

test_that("optional loaders and failure/retry behave as for attached access", {
  local_clean_session()
  fx <- shared_fixtures()
  calls <- 0
  data_loader_add(data_loader("test::flaky", open = function(descriptor, paths, context) {
    calls <<- calls + 1
    if (calls == 1) stop("backing file temporarily unavailable")
    list(flaky_obj = "recovered")
  }))
  expect_error(data_get("fancy_obj", package = "pdref", lib.loc = fx$lib),
               class = "projectData_loader_unavailable")
  expect_identical(data_get("ref_rds", package = "pdref")$value, "rds-object")

  expect_error(data_get("flaky_obj", package = "pdmisc", lib.loc = fx$lib),
               class = "projectData_opening_failed")
  expect_error(data_get("flaky_obj", package = "pdmisc"), "data_retry",
               class = "projectData_opening_failed")
  expect_equal(calls, 1)
  expect_identical(data_get("plain_obj", package = "pdmisc"), 1)
  expect_equal(data_retry("pdmisc"), "flaky")
  expect_identical(data_get("flaky_obj", package = "pdmisc"), "recovered")
  expect_equal(calls, 2)
  # The failed-then-recovered resource is shared with attached access.
  data_attach("pdmisc")
  expect_identical(flaky_obj, "recovered")
  expect_equal(calls, 2)
})

test_that("cleanup of unattached registrations follows the documented lifecycle", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  sp <- search()
  data_get("cds_main", package = "pdmain", lib.loc = fx$lib)
  expect_error(data_primary("pdmain"), "not attached")
  data_prefer("cds_main", "pdmain") |> expect_error(class = "projectData_bad_preference")

  expect_identical(data_detach("pdmain"), "pdmain")
  expect_equal(nrow(data_collections()), 0L)
  expect_identical(search(), sp)
  # Installed files are never touched.
  expect_true(file.exists(file.path(fx$lib, "pdmain", "Meta", "package.rds")))
  # Forgetting drops the cache: the next access registers and opens again.
  data_get("cds_main", package = "pdmain", lib.loc = fx$lib)
  expect_identical(n$log, c("pdmain/cds_main", "pdmain/cds_main"))

  # Detaching the last attached collection removes the environment but keeps
  # unattached registrations (and their caches).
  data_attach("pdref", lib.loc = fx$lib)
  expect_true("projectData:data" %in% search())
  data_detach("pdref")
  expect_identical(search(), sp)
  expect_equal(data_collections()$id, "pdmain")
  data_get("cds_main", package = "pdmain")
  expect_length(n$log, 2L)
  expect_identical(data_detach(), "pdmain")
  expect_equal(nrow(data_collections()), 0L)
})

test_that("data_detach() of an attached collection does not disturb unattached ones", {
  local_clean_session()
  fx <- shared_fixtures()
  data_get("only_ref", package = "pdref", lib.loc = fx$lib)
  data_attach("pdmain", lib.loc = fx$lib)
  data_detach("pdmain")
  expect_false("projectData:data" %in% search())
  expect_equal(data_collections()$id, "pdref")
  expect_identical(data_get("only_ref", package = "pdref"), 3)
})

test_that("detaching an unattached disk-backed registration leaves backing files and live objects intact", {
  local_clean_session()
  fx <- shared_fixtures()
  lib <- fixture_library()
  fixture_package(lib, "pdwork", lazy = FALSE, datasets = list(),
    files = list("extdata/w.tinymat" = function(p) tinymat_write(matrix(as.double(1:6), 2, 3), p)),
    manifest = c("ManifestVersion: 1", "", "Resource: w", "Loader: tinydisk::tinymat",
                 "Source: extdata/w.tinymat", "Objects: w", "Options: work_copy=true"))
  data_loader_add(tinydisk_loader())
  n <- local_open_counter()
  sp <- search()

  # In-place backing files (installed library) and a private work copy.
  mat <- data_get("mat", package = "pdref", lib.loc = fx$lib)
  w <- data_get("w", package = "pdwork", lib.loc = lib)
  expect_identical(search(), sp)
  expect_false(any(data_collections()$attached))
  expect_s3_class(mat, "tinymat")
  expect_true(startsWith(mat$path, normalizePath(fx$lib, winslash = "/")))
  expect_true(grepl("projectData-work", w$path))
  expect_setequal(n$log, c("pdref/mat", "pdwork/w"))

  expect_setequal(data_detach(), c("pdref", "pdwork"))
  expect_equal(nrow(data_collections()), 0L)
  expect_true(file.exists(file.path(mat$path, "values.bin")))
  expect_true(file.exists(file.path(w$path, "values.bin")))
  expect_equal(mat[2, 3], 8)
  expect_equal(w[2, 3], 6)

  # Even with the source package removed, the work copy stays valid.
  unlink(file.path(lib, "pdwork"), recursive = TRUE)
  expect_true(file.exists(file.path(w$path, "values.bin")))
  expect_equal(w[, 3], c(5, 6))

  # Forgetting dropped the cache: explicit access opens again.
  mat2 <- data_get("mat", package = "pdref", lib.loc = fx$lib)
  expect_equal(mat2[2, 3], 8)
  expect_equal(sum(n$log == "pdref/mat"), 2L)
})

test_that("add_collections() names whether a refused id is attached or only registered", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdmain", lib.loc = fx$lib)
  data_get("only_ref", package = "pdref", lib.loc = fx$lib)
  cand <- projectData:::snapshot()
  expect_error(
    projectData:::add_collections(cand, "pdmain", fx$lib, "pdmain", NULL, NULL, replace = FALSE),
    "'pdmain' \\(attached\\)"
  )
  expect_error(
    projectData:::add_collections(cand, "pdref", fx$lib, "pdref", NULL, NULL, replace = FALSE),
    "already registered: 'pdref' \\(registered\\)"
  )
  # Through the public path: attached ids need replace, unattached ones do not.
  expect_error(data_attach("pdmain", lib.loc = fx$lib, suffix = ".m"), "already attached: 'pdmain' \\(attached\\)")
  expect_no_error(data_attach("pdref", lib.loc = fx$lib, suffix = ".r"))
})
