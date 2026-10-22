test_that("manifest-described RDS objects load through the built-in loader", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_attach("pdref", lib.loc = fx$lib)
  cat <- data_catalog()
  r <- cat[cat$object %in% "ref_rds", ]
  expect_equal(r$mapping, "declared")
  expect_equal(r$recorded_class, "list")
  expect_equal(r$source, "extdata/refobj.rds")
  expect_length(n$log, 0)
  expect_identical(data_get("ref_rds"), list(value = "rds-object"))
  expect_equal(n$log, "pdref/refobj")
})

test_that("extdata files without a manifest entry or discovering loader are not exposed", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib)
  expect_false("mat" %in% data_catalog()$object)
})

test_that("missing adapters block only their own objects", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib)
  cat <- data_catalog()
  expect_equal(cat$state[cat$object %in% "fancy_obj"], "unavailable-loader")
  expect_error(data_get("fancy_obj"), "notinstalledfancypkg", class = "projectData_loader_unavailable")
  expect_identical(data_get("only_ref"), 3)
  expect_equal(data_catalog()$state[data_catalog()$object %in% "fancy_obj"], "unavailable-loader")
})

test_that("loader ids, contract, replacement and removal behave as documented", {
  local_clean_session()
  mk <- function(v) data_loader("test::fmt", open = function(descriptor, paths, context) list(x = v))
  expect_error(data_loader("nonamespace", open = identity), "namespaced")
  expect_error(data_loader_add(data_loader("projectData::mine", open = identity)), "reserved")
  data_loader_add(mk(1))
  expect_error(data_loader_add(mk(2)), "already registered")
  data_loader_add(mk(2), replace = TRUE)
  expect_true("test::fmt" %in% data_loaders()$id)
  bad <- mk(1); bad$contract <- 2L
  expect_error(data_loader_add(bad, replace = TRUE), "contract version")
  expect_error(data_loader_remove("projectData::dataset"), "Built-in")
  data_loader_remove("test::fmt")
  expect_false("test::fmt" %in% data_loaders()$id)
  expect_error(data_loader_adapter("notinstalledfancypkg"), class = "projectData_loader_unavailable")
})

test_that("removing a loader leaves ready objects usable and marks unopened ones unavailable", {
  local_clean_session()
  fx <- shared_fixtures()
  data_loader_add(data_loader("test::flaky", open = function(descriptor, paths, context) list(flaky_obj = "ok")))
  data_loader_add(data_loader("test::reenter", open = function(descriptor, paths, context) list(reenter_obj = "ok2")))
  data_attach("pdmisc", lib.loc = fx$lib)
  expect_identical(data_get("flaky_obj"), "ok")
  data_loader_remove("test::flaky"); data_loader_remove("test::reenter")
  expect_identical(data_get("flaky_obj"), "ok")
  cat <- data_catalog()
  expect_equal(cat$state[cat$object %in% "reenter_obj"], "unavailable-loader")
  expect_error(data_get("reenter_obj"), class = "projectData_loader_unavailable")
})

test_that("a disk-backed backend is discovered, opened lazily from disk, and not copied into memory", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_loader_add(tinydisk_loader())
  data_attach("pdref", lib.loc = fx$lib)
  cat <- data_catalog()
  m <- cat[cat$object %in% "mat", ]
  expect_equal(c(m$storage, m$state, m$mapping), c("disk-backed", "unopened", "index"))
  expect_length(n$log, 0)
  mat <- data_get("mat")
  expect_s3_class(mat, "tinymat")
  expect_equal(dim(mat), c(3L, 4L))
  expect_equal(mat[2, 3], 8)
  expect_equal(mat[, 4], c(10, 11, 12))
  expect_true(startsWith(mat$path, normalizePath(fx$lib, winslash = "/")))
  expect_false(any(vapply(unclass(mat), is.numeric, NA) & lengths(unclass(mat)) > 2))
  expect_equal(n$log, "pdref/mat")
})

test_that("discovery precedence: manifest overrides, ambiguous discovery errors", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdtm", lazy = FALSE, datasets = list(),
    files = list("extdata/a.tinymat" = function(p) tinymat_write(diag(2), p)))
  # two adapters claiming the same resource id
  other <- tinydisk_loader()
  other$id <- "tinydisk::second"
  data_loader_add(tinydisk_loader())
  data_loader_add(other)
  expect_error(data_attach("pdtm", lib.loc = lib), class = "projectData_ambiguous_discovery")
  expect_length(data_collections()$id, 0)
})

test_that("a manifest entry replaces an adapter-discovered candidate", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdtm2", lazy = FALSE, datasets = list(),
    files = list("extdata/a.tinymat" = function(p) tinymat_write(diag(2), p)),
    manifest = c("ManifestVersion: 1", "", "Resource: a", "Loader: tinydisk::tinymat",
                 "Source: extdata/a.tinymat", "Objects: renamed_a"))
  data_loader_add(tinydisk_loader())
  data_attach("pdtm2", lib.loc = lib)
  expect_equal(data_catalog()$object, "renamed_a")
  expect_equal(data_catalog()$mapping, "declared")
})

test_that("work directories are distinct, writable copies survive source removal", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdwork", lazy = FALSE, datasets = list(),
    files = list("extdata/w.tinymat" = function(p) tinymat_write(matrix(as.double(1:6), 2, 3), p)),
    manifest = c("ManifestVersion: 1", "", "Resource: w", "Loader: tinydisk::tinymat",
                 "Source: extdata/w.tinymat", "Objects: w", "Options: work_copy=true"))
  data_loader_add(tinydisk_loader())
  data_attach("pdwork", lib.loc = lib)
  w <- data_get("w")
  expect_false(startsWith(w$path, normalizePath(lib, winslash = "/")))
  expect_true(grepl("projectData-work", w$path))
  data_detach("pdwork")
  unlink(file.path(lib, "pdwork"), recursive = TRUE)
  expect_equal(w[2, 3], 6)
})

test_that("relocating the library: backing files follow the registered location", {
  local_clean_session()
  fx <- shared_fixtures()
  data_loader_add(tinydisk_loader())
  moved <- fixture_library()
  file.copy(file.path(fx$lib, "pdref"), moved, recursive = TRUE)
  data_attach("pdref", lib.loc = fx$lib, id = "orig")
  data_attach("pdref", lib.loc = moved, id = "moved")
  m <- data_get("mat", "moved")
  expect_true(startsWith(m$path, normalizePath(moved, winslash = "/")))
  expect_equal(m[1, 1], 1)
  # The original registration still points at the original location.
  o <- data_get("mat", "orig")
  expect_true(startsWith(o$path, normalizePath(fx$lib, winslash = "/")))
  # Backing files are not protected: removing them breaks the opened object.
  unlink(file.path(moved, "pdref"), recursive = TRUE)
  expect_error(suppressWarnings(m[1, 1]))
  expect_equal(o[1, 1], 1)
})

test_that("manifest validation rejects unsafe paths, bad versions and duplicates", {
  mf <- function(...) {
    root <- tempfile("root"); dir.create(file.path(root, "projectData"), recursive = TRUE)
    writeLines(c(...), file.path(root, "projectData", "manifest.dcf"))
    dir.create(file.path(root, "extdata")); saveRDS(1, file.path(root, "extdata", "x.rds"))
    file.path(root, "projectData", "manifest.dcf")
  }
  rec <- function(src) c("Resource: r", "Loader: projectData::rds", paste("Source:", src), "Objects: o")
  expect_error(data_manifest_validate(mf("ManifestVersion: 1", "", rec("../x.rds"))), "relative path")
  expect_error(data_manifest_validate(mf("ManifestVersion: 1", "", rec("/etc/passwd"))), "relative path")
  expect_error(data_manifest_validate(mf("ManifestVersion: 1", "", rec("C:/x"))), "relative path")
  expect_error(data_manifest_validate(mf("ManifestVersion: 1", "", rec("extdata\\x.rds"))), "relative path")
  expect_error(data_manifest_validate(mf("ManifestVersion: 1", "", rec("extdata/missing.rds"))), "does not exist")
  expect_error(data_manifest_validate(mf("ManifestVersion: 2", "", rec("extdata/x.rds"))), "unsupported")
  expect_error(data_manifest_validate(mf("Resource: r", "Loader: projectData::rds")), "ManifestVersion")
  expect_error(data_manifest_validate(mf("ManifestVersion: 1", "", rec("extdata/x.rds"), "", rec("extdata/x.rds"))), "duplicate")
  expect_error(data_manifest_validate(mf("ManifestVersion: 1", "", c(rec("extdata/x.rds"), "Code: system('x')"))), "unknown field")
  expect_error(data_manifest_validate(mf("ManifestVersion: 1", "", "Resource: r", "Loader: projectData::rds", "Source: extdata/x.rds")), "exactly one")
  expect_error(data_manifest_validate(mf("ManifestVersion: 1", "", "Resource: r", "Loader: notnamespaced")), "namespaced")
  expect_no_error(data_manifest_validate(mf("ManifestVersion: 1", "", rec("extdata/x.rds"))))
})

test_that("messages announce opening and distinguish local files from downloads", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib)
  data_config(messages = TRUE)
  expect_message(data_get("only_ref"), "nothing is downloaded")
  expect_no_message(data_get("only_ref"))
})

test_that("data_get() with several names opens each needed resource once", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_attach("pdmain", lib.loc = fx$lib)
  data_get("meta_a")
  expect_equal(n$log, "pdmain/meta")
  # several names in one call; the sibling meta_b shares the already-open resource
  vals <- data_get(c("meta_b", "cds_main", "pair_x"))
  expect_equal(names(vals), c("meta_b", "cds_main", "pair_x"))
  expect_equal(sort(n$log), sort(c("pdmain/meta", "pdmain/cds_main", "pdmain/pair")))
})

test_that("script-based datasets run once, on first access, in an isolated environment", {
  local_clean_session()
  marker <- tempfile("pd-marker-")
  withr::local_options(pd.fixture.marker = marker)
  withr::defer(unlink(marker))
  lib <- fixture_library()
  withr::defer(unlink(lib, recursive = TRUE))
  fixture_package(lib, "pdscript", "0.1.0", lazy = FALSE,
    scripts = list(treatment_groups = c(
      # R CMD INSTALL also evaluates data scripts (to build the index), when the option is unset
      "if (!is.null(getOption('pd.fixture.marker'))) cat('run\\n', file = getOption('pd.fixture.marker'), append = TRUE)",
      "script_helper_levels <- c('control', 'low', 'high')",
      "treatment_groups <- data.frame(",
      "  id = c(1, 2, 3, 4),",
      "  group = factor(c('control', 'low', 'high', 'low'), levels = script_helper_levels)",
      ")"
    )),
    script_docs = list(treatment_groups = "treatment_groups"))
  runs <- function() if (file.exists(marker)) length(readLines(marker)) else 0L
  glob_before <- ls(globalenv(), all.names = TRUE)
  n <- local_open_counter()

  data_query("pdscript", lib.loc = lib)
  data_attach("pdscript", lib.loc = lib)
  expect_true("treatment_groups" %in% data_catalog()$object)
  expect_equal(runs(), 0L)
  expect_length(n$log, 0)

  tg <- data_get("treatment_groups")
  expect_equal(runs(), 1L)
  expect_s3_class(tg, "data.frame")
  expect_equal(nrow(tg), 4L)
  expect_equal(levels(tg$group), c("control", "low", "high"))
  expect_equal(n$log, "pdscript/treatment_groups")

  expect_identical(data_get("treatment_groups"), tg)
  expect_equal(runs(), 1L)
  expect_length(n$log, 1)

  expect_identical(ls(globalenv(), all.names = TRUE), glob_before)
  expect_false(exists("treatment_groups", envir = globalenv(), inherits = FALSE))
  expect_false(exists("script_helper_levels", envir = globalenv(), inherits = FALSE))
})

test_that("script datasets calling unqualified non-base functions fail clearly, and pkg:: works", {
  local_clean_session()
  lib <- fixture_library()
  withr::defer(unlink(lib, recursive = TRUE))
  # Installation evaluates scripts with default packages attached, so both install cleanly.
  fixture_package(lib, "pdscriptfn", "0.1.0", lazy = FALSE,
    scripts = list(
      unqualified = "unqualified <- head(letters, 3)",
      qualified = "qualified <- utils::head(letters, 3)"
    ))
  data_attach("pdscriptfn", lib.loc = lib)

  expect_error(data_get("unqualified"), "could not find function \"head\"")
  cat <- data_catalog()
  expect_equal(cat$state[cat$object %in% "unqualified"], "failed")
  expect_false(exists("unqualified", envir = globalenv(), inherits = FALSE))

  expect_identical(data_get("qualified"), letters[1:3])
  expect_equal(data_catalog()$state[data_catalog()$object %in% "qualified"], "ready")
})
