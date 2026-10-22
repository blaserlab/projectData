test_that("manifest-described RDS objects load through the built-in loader", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_attach("pdref", lib.loc = fx$lib)
  cat <- data_catalog()
  r <- cat[cat$object %in% "ref_rds", ]
  expect_equal(c(r$origin, r$mapping), c("manifest", "recorded"))
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
  expect_equal(c(m$storage, m$state, m$mapping), c("disk-backed", "unopened", "recorded"))
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
  expect_equal(data_catalog()[c("origin", "mapping")] |> as.list() |> unlist() |> unname(), c("manifest", "recorded"))
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

test_that("messages announce opening without promising anything about reader behavior", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib)
  data_config(messages = TRUE)
  expect_message(data_get("only_ref"), "does not install packages")
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

# Discovery collisions and manifest resolution -------------------------------

second_loader <- function() {
  l <- tinydisk_loader()
  l$id <- "tinydisk::second"
  l
}

tm_file <- function(p) tinymat_write(diag(2), p)

test_that("an adapter claim on a standard dataset is ignored; the built-in reader keeps it", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdstd", lazy = FALSE, datasets = list(std = list(std_obj = 1)),
                  files = list("extdata/std.tinymat" = tm_file))
  data_loader_add(tinydisk_loader())
  expect_warning(data_attach("pdstd", lib.loc = lib), class = "projectData_ignored_claim")
  cat <- data_catalog()
  expect_equal(cat$object, "std_obj")
  expect_equal(data_get("std_obj"), 1)
})

test_that("ignored adapter claims can be downgraded to a message or silenced", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdstd3", lazy = FALSE, datasets = list(std = list(std_obj = 1)),
                  files = list("extdata/std.tinymat" = tm_file))
  data_loader_add(tinydisk_loader())
  expect_error(data_config(ignored_claims = "loud"))
  data_config(ignored_claims = "message")
  expect_message(data_attach("pdstd3", lib.loc = lib, id = "m"), class = "projectData_ignored_claim")
  expect_equal(data_catalog()$object, "std_obj")
  data_config(ignored_claims = "silent")
  expect_no_condition(data_attach("pdstd3", lib.loc = lib, id = "s"))
  expect_setequal(data_collections()$id, c("m", "s"))
})

test_that("a manifest can assign a standard dataset to a specialized reader", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdstd2", lazy = FALSE, datasets = list(std = list(std_obj = 1)),
                  files = list("extdata/std.tinymat" = tm_file),
                  manifest = c("ManifestVersion: 1", "", "Resource: std",
                               "Loader: tinydisk::tinymat", "Source: extdata/std.tinymat",
                               "Objects: std_mat"))
  data_loader_add(tinydisk_loader())
  expect_no_warning(data_attach("pdstd2", lib.loc = lib))
  expect_equal(data_catalog()$object, "std_mat")
  expect_s3_class(data_get("std_mat"), "tinymat")
})

test_that("a manifest resolves a collision between adapters on a nonstandard resource", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdadp", lazy = FALSE, datasets = list(),
                  files = list("extdata/a.tinymat" = tm_file),
                  manifest = c("ManifestVersion: 1", "", "Resource: a",
                               "Loader: tinydisk::second", "Source: extdata/a.tinymat",
                               "Objects: a"))
  data_loader_add(tinydisk_loader())
  data_loader_add(second_loader())
  data_attach("pdadp", lib.loc = lib)
  expect_equal(data_catalog()$object, "a")
  expect_equal(projectData:::.state$collections[[1]]$resources$a$descriptor$loader,
               "tinydisk::second")
})

test_that("the same adapter collision without a resolving manifest names the claimants", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdadp2", lazy = FALSE, datasets = list(),
                  files = list("extdata/a.tinymat" = tm_file))
  data_loader_add(tinydisk_loader())
  data_loader_add(second_loader())
  expect_error(data_attach("pdadp2", lib.loc = lib), "a \\(adapter:tinydisk::second\\)",
               class = "projectData_ambiguous_discovery")
  expect_error(data_attach("pdadp2", lib.loc = lib), "a \\(adapter:tinydisk::tinymat\\)")
})

test_that("a manifest for an unrelated resource does not suppress the collision", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdadp3", lazy = FALSE, datasets = list(),
                  files = list("extdata/a.tinymat" = tm_file,
                               "extdata/refobj.rds" = function(p) saveRDS(1, p)),
                  manifest = c("ManifestVersion: 1", "", "Resource: refobj",
                               "Loader: projectData::rds", "Source: extdata/refobj.rds",
                               "Objects: ref_rds"))
  data_loader_add(tinydisk_loader())
  data_loader_add(second_loader())
  expect_error(data_attach("pdadp3", lib.loc = lib), class = "projectData_ambiguous_discovery")
})

test_that("resolution does not depend on adapter registration order", {
  lib <- fixture_library()
  fixture_package(lib, "pdord", lazy = FALSE, datasets = list(std = list(std_obj = 1)),
                  files = list("extdata/std.tinymat" = tm_file, "extdata/a.tinymat" = tm_file,
                               "extdata/b.tinymat" = tm_file),
                  manifest = c("ManifestVersion: 1", "", "Resource: a",
                               "Loader: tinydisk::second", "Source: extdata/a.tinymat",
                               "Objects: a"))
  resolve <- function(loaders) {
    local_clean_session()
    for (l in loaders) data_loader_add(l)
    msg <- tryCatch(suppressWarnings(data_attach("pdord", lib.loc = lib)),
                    projectData_ambiguous_discovery = function(e) conditionMessage(e))
    list(msg = if (is.character(msg)) msg, cat = if (!is.character(msg)) data_catalog()$object)
  }
  fwd <- resolve(list(tinydisk_loader(), second_loader()))
  rev <- resolve(list(second_loader(), tinydisk_loader()))
  expect_identical(fwd, rev)
  # b is claimed by both adapters and nothing resolves it
  expect_match(fwd$msg, "b \\(adapter")
})

test_that("duplicate or contradictory manifest entries fail clearly", {
  local_clean_session()
  lib <- fixture_library()
  rec <- function(loader) c("Resource: a", paste("Loader:", loader),
                            "Source: extdata/a.tinymat", "Objects: a")
  for (m in list(c("ManifestVersion: 1", "", rec("tinydisk::tinymat"), "", rec("tinydisk::tinymat")),
                 c("ManifestVersion: 1", "", rec("tinydisk::tinymat"), "", rec("tinydisk::second")))) {
    nm <- paste0("pddup", length(list.files(lib)))
    fixture_package(lib, nm, lazy = FALSE, datasets = list(),
                    files = list("extdata/a.tinymat" = tm_file), manifest = m)
    expect_error(data_attach(nm, lib.loc = lib), "duplicate Resource 'a'",
                 class = "projectData_manifest_error")
  }
})

test_that("an adapter still discovers a directory resource containing internal .rda files", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdinner", lazy = FALSE, datasets = list(std = list(std_obj = 1)),
                  files = list("extdata/inner.tinymat" = function(p) {
                    tinymat_write(diag(2), p)
                    x <- 1
                    save(x, file = file.path(p, "extra.rda"))
                  }))
  data_loader_add(tinydisk_loader())
  expect_no_warning(data_attach("pdinner", lib.loc = lib))
  expect_setequal(data_catalog()$object, c("std_obj", "inner"))
})

test_that("discovery and manifest resolution do not open resources", {
  local_clean_session()
  lib <- fixture_library()
  opened <- 0L
  spy <- function(id) {
    l <- tinydisk_loader()
    l$id <- id
    l$open <- function(descriptor, paths, context) { opened <<- opened + 1L; list() }
    l
  }
  fixture_package(lib, "pdspy", lazy = FALSE, datasets = list(std = list(std_obj = 1)),
                  files = list("extdata/std.tinymat" = tm_file, "extdata/a.tinymat" = tm_file),
                  manifest = c("ManifestVersion: 1", "", "Resource: a",
                               "Loader: tinydisk::spy2", "Source: extdata/a.tinymat",
                               "Objects: a"))
  n <- local_open_counter()
  data_loader_add(spy("tinydisk::spy1"))
  data_loader_add(spy("tinydisk::spy2"))
  suppressWarnings(data_attach("pdspy", lib.loc = lib))
  data_catalog()
  expect_equal(opened, 0L)
  expect_length(n$log, 0)
})
