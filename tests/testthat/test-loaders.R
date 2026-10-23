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

# Startup-package lookup for script datasets (issue 2.5) -----------------------

std_script_library <- function(env = parent.frame()) {
  lib <- fixture_library()
  withr::defer(unlink(lib, recursive = TRUE), envir = env)
  fixture_package(lib, "pdhelper", "0.1.0",
    code = "pdhelper_fn <- function() 'from-helper'", exports = "pdhelper_fn")
  fixture_package(lib, "pdstd", "0.1.0", lazy = FALSE,
    scripts = list(
      basic = c(
        "basic_df <- data.frame(g = factor(c('a', 'b', 'a', 'b')), x = c(1, 2, 3, 5))",
        "basic_head <- head(basic_df, 2)",
        "basic_design <- model.matrix(~ g + x, basic_df)",
        "basic_fit <- lm(x ~ g, basic_df)",
        "basic_pred <- predict(basic_fit, data.frame(g = 'b'))",
        "basic_cars <- nrow(mtcars)"
      ),
      overlap = c("overlap_kron <- kronecker", "overlap_plot <- plot"),
      shadow = c(
        "shadow_res <- list(head = head(letters, 2),",
        "  mm = dim(model.matrix(~ x, data.frame(x = 1:3))), mt = nrow(mtcars))"
      ),
      graphics_check = "graphics_check <- is.function(par)",
      bare_helper = "bare_helper <- exists('pdhelper_fn')",
      # Guarded because R CMD INSTALL evaluates scripts and may not see the fixture library
      qualified_helper = "qualified_helper <- if (requireNamespace('pdhelper', quietly = TRUE)) pdhelper::pdhelper_fn() else NA"
    ),
    script_docs = list(
      basic = c("basic_df", "basic_head", "basic_design", "basic_fit", "basic_pred", "basic_cars"),
      overlap = c("overlap_kron", "overlap_plot"),
      shadow = "shadow_res", graphics_check = "graphics_check",
      bare_helper = "bare_helper", qualified_helper = "qualified_helper"
    ))
  lib
}

test_that("script datasets can call standard startup package functions unqualified", {
  local_clean_session()
  lib <- std_script_library()
  data_attach("pdstd", lib.loc = lib)

  expect_equal(data_get("basic_head")$x, c(1, 2))
  expect_equal(dim(data_get("basic_design")), c(4L, 3L))
  expect_equal(colnames(data_get("basic_design")), c("(Intercept)", "gb", "x"))
  expect_equal(data_get("basic_cars"), 32L)
  # S3 dispatch on an object created in the script
  expect_s3_class(data_get("basic_fit"), "lm")
  expect_equal(unname(data_get("basic_pred")),
               unname(predict(lm(x ~ g, data.frame(g = factor(c("a", "b", "a", "b")), x = c(1, 2, 3, 5))),
                              data.frame(g = "b"))))
})

test_that("script lookup follows startup precedence, and only the fixed set is used", {
  local_clean_session()
  lib <- std_script_library()
  data_attach("pdstd", lib.loc = lib)

  # Names exported by several standard packages resolve like an ordinary session
  expect_identical(data_get("overlap_kron"), methods::kronecker)
  expect_identical(data_get("overlap_plot"), graphics::plot)

  pkgs <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods")
  expect_identical(projectData:::script_packages, pkgs)
  exports <- lapply(pkgs, projectData:::standard_export_names)
  names(exports) <- pkgs
  exports$base <- ls(baseenv(), all.names = TRUE)
  pkgs_all <- c(pkgs, "base")
  env <- projectData:::script_lookup_env()
  dup <- names(which(table(unlist(exports)) > 1))
  expect_true(all(c("kronecker", "plot") %in% dup))
  for (nm in dup) {
    first <- pkgs_all[vapply(exports, function(e) nm %in% e, logical(1))][1]
    expected <- if (first == "base") get(nm, envir = baseenv()) else getExportedValue(first, nm)
    expect_identical(get(nm, envir = env), expected, info = nm)
  }
  # The chain ends in base, and .GlobalEnv is never on it
  chain <- list()
  for (e in list(env)) {
    while (!identical(e, emptyenv())) { chain <- c(chain, list(e)); e <- parent.env(e) }
  }
  expect_length(chain, length(pkgs) + 1L)
  expect_identical(chain[[length(chain)]], baseenv())
  expect_false(any(vapply(chain, identical, logical(1), globalenv())))

  # Customized startup settings do not change the supported set
  withr::local_options(defaultPackages = "base")
  expect_equal(data_get("basic_cars"), 32L)
  expect_equal(nrow(data_get("basic_head")), 2L)
})

test_that("workspace and attached data cannot affect script results", {
  local_clean_session()
  lib <- std_script_library()
  fixture_package(lib, "pdshadow", "0.1.0", lazy = FALSE,
    datasets = list(shadow_objs = list(head = "attached-head", model.matrix = "attached-mm")))
  data_attach(c("pdstd", "pdshadow"), lib.loc = lib, on_mask = "ignore")

  assign("head", function(...) stop("global head used"), envir = globalenv())
  assign("model.matrix", function(...) stop("global model.matrix used"), envir = globalenv())
  assign("mtcars", "global-mtcars", envir = globalenv())
  withr::defer(rm(list = c("head", "model.matrix", "mtcars"), envir = globalenv()))

  res <- data_get("shadow_res", collection = "pdstd")
  expect_equal(res$head, c("a", "b"))
  expect_equal(res$mm, c(3L, 2L))
  expect_equal(res$mt, 32L)
})

test_that("additional attached packages are not visible unqualified, but qualified calls work", {
  local_clean_session()
  lib <- std_script_library()
  withr::local_libpaths(lib, action = "prefix")
  suppressPackageStartupMessages(library("pdhelper", lib.loc = lib))
  withr::defer(detach("package:pdhelper", unload = TRUE, character.only = TRUE))
  expect_true(is.function(pdhelper_fn))
  data_attach("pdstd", lib.loc = lib)

  expect_true("package:pdhelper" %in% search())
  expect_false(data_get("bare_helper"))
  expect_error(eval(quote(pdhelper_fn()), new.env(parent = projectData:::script_lookup_env())),
               "could not find function \"pdhelper_fn\"")
  expect_identical(data_get("qualified_helper"), "from-helper")
  expect_equal(data_catalog()$state[data_catalog()$object %in% "qualified_helper"], "ready")
})

test_that("opening a script dataset leaves the search path unchanged", {
  local_clean_session()
  lib <- std_script_library()
  data_attach("pdstd", lib.loc = lib)

  before <- search()
  expect_equal(data_get("basic_cars"), 32L)
  expect_identical(search(), before)

  # a standard package that is not attached is still usable, and stays detached
  pos <- match("package:graphics", search())
  detach("package:graphics")
  withr::defer(suppressWarnings(library("graphics", pos = pos, character.only = TRUE)))
  before <- search()
  expect_true(data_get("graphics_check"))
  expect_identical(search(), before)
  expect_false("package:graphics" %in% search())
})

test_that("building the lookup environment retrieves no values", {
  n <- 0L
  counting <- function(pkg, name) {
    n <<- n + 1L
    getExportedValue(pkg, name)
  }
  env <- projectData:::script_lookup_env(counting)
  expect_equal(n, 0L)
  expect_true(exists("mtcars", envir = env) && exists("median", envir = env) &&
                exists("setClass", envir = env))
  expect_equal(n, 0L)
  expect_equal(eval(quote(head(1:5, 2)), new.env(parent = env)), 1:2)
  expect_equal(n, 1L)
  # Construction does not run during inventory or attachment
  local_clean_session()
  lib <- std_script_library()
  calls <- new.env()
  calls$n <- 0L
  bump <- function() calls$n <- calls$n + 1L
  suppressMessages(trace("script_lookup_env", bquote(.(bump)()), print = FALSE,
                         where = asNamespace("projectData")))
  withr::defer(suppressMessages(untrace("script_lookup_env", where = asNamespace("projectData"))))
  data_query("pdstd", lib.loc = lib)
  data_attach("pdstd", lib.loc = lib)
  data_catalog()
  expect_equal(calls$n, 0L)
  data_get("basic_cars")
  expect_equal(calls$n, 1L)
})

test_that("script outputs contain only what the script assigned", {
  local_clean_session()
  lib <- std_script_library()
  data_attach("pdstd", lib.loc = lib)
  expect_warning(data_inspect("pdstd", "basic"), class = "projectData_inspection")
  expect_setequal(
    data_catalog()$object[data_catalog()$resource %in% "basic"],
    c("basic_df", "basic_head", "basic_design", "basic_fit", "basic_pred", "basic_cars")
  )
  expect_false(any(c("head", "model.matrix", "mtcars", "lm") %in% data_catalog()$object))
  expect_false(exists("basic_df", envir = globalenv(), inherits = FALSE))
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

# Issue 2.6: storage of a ready resource comes from its opening, not the registry.
local_tinydisk_lib <- function(names, env = parent.frame()) {
  lib <- fixture_library()
  fixture_package(lib, "pdsnap", lazy = FALSE, datasets = list(),
    files = stats::setNames(
      lapply(names, function(i) function(p) tinymat_write(matrix(as.double(1:6), 2, 3), p)),
      paste0("extdata/", names, ".tinymat")))
  lib
}

memory_variant <- function(counts, fail = FALSE) {
  l <- tinydisk_loader()
  o <- l$open
  l$storage <- "memory"
  l$open <- function(descriptor, paths, context) {
    counts$open <- counts$open + 1L
    if (fail) stop("memory loader failure")
    o(descriptor, paths, context)
  }
  l
}

test_that("ready storage survives loader removal and replacement; unopened follows the registry", {
  local_clean_session()
  lib <- local_tinydisk_lib(c("first", "second"))
  cl <- counting_tinydisk()
  n <- local_open_counter()
  data_loader_add(cl$loader)
  data_attach("pdsnap", lib.loc = lib)
  st <- function() {
    cat <- data_catalog()
    stats::setNames(cat$storage, cat$object)
  }
  expect_equal(st(), c(first = "disk-backed", second = "disk-backed"))

  m1 <- data_get("first")
  expect_equal(cl$counts$open, 1L)
  rec <- projectData:::get_collection("pdsnap")$resources$first
  expect_equal(rec$opened_with,
               list(id = "tinydisk::tinymat", contract = 1L, storage = "disk-backed"))
  expect_null(projectData:::get_collection("pdsnap")$resources$second$opened_with)

  data_loader_remove("tinydisk::tinymat")
  cat <- data_catalog()
  expect_equal(cat$storage[cat$object == "first"], "disk-backed")
  expect_equal(cat$loader_status[cat$object == "first"], "unavailable")
  expect_equal(cat$state[cat$object == "first"], "ready")
  expect_equal(cat$storage[cat$object == "second"], "unknown")
  expect_equal(cat$state[cat$object == "second"], "unavailable-loader")
  expect_identical(data_get("first"), m1)
  expect_equal(cl$counts$open, 1L)

  mem <- new.env(); mem$open <- 0L
  data_loader_add(memory_variant(mem))
  expect_equal(st(), c(first = "disk-backed", second = "memory"))
  cat <- data_catalog()
  expect_equal(cat$loader_status[cat$object == "first"], "registered")
  expect_identical(data_get("first"), m1)
  expect_equal(c(cl$counts$open, mem$open), c(1L, 0L))
  expect_equal(n$log, "pdsnap/first")

  m2 <- data_get("second")
  expect_equal(mem$open, 1L)
  expect_equal(projectData:::get_collection("pdsnap")$resources$second$opened_with$storage, "memory")
  data_loader_remove("tinydisk::tinymat")
  expect_equal(st(), c(first = "disk-backed", second = "memory"))
  expect_equal(c(cl$counts$open, mem$open), c(1L, 1L))
})

test_that("a failed opening keeps no snapshot; a retry records the loader used", {
  local_clean_session()
  lib <- local_tinydisk_lib("only")
  mem <- new.env(); mem$open <- 0L
  data_loader_add(memory_variant(mem, fail = TRUE))
  data_attach("pdsnap", lib.loc = lib)
  rec <- projectData:::get_collection("pdsnap")$resources$only
  expect_error(data_get("only"), class = "projectData_opening_failed")
  expect_null(rec$opened_with)
  cat <- data_catalog()
  expect_equal(c(cat$state, cat$storage), c("failed", "memory"))

  cl <- counting_tinydisk()
  data_loader_add(cl$loader, replace = TRUE)
  data_retry("pdsnap", "only")
  expect_s3_class(data_get("only"), "tinymat")
  expect_equal(rec$opened_with$storage, "disk-backed")
  data_loader_add(memory_variant(mem), replace = TRUE)
  expect_equal(data_catalog()$storage, "disk-backed")
  expect_equal(c(mem$open, cl$counts$open), c(1L, 1L))
})

test_that("a mapping mismatch still records opening metadata and stays inspectable", {
  local_clean_session()
  lib <- fixture_library()
  fixture_package(lib, "pdsnapmm", lazy = FALSE, datasets = list(),
    files = list("extdata/real.tinymat" = function(p) tinymat_write(diag(2), p)),
    manifest = c("ManifestVersion: 1", "", "Resource: real", "Loader: tinydisk::tinymat",
                 "Source: extdata/real.tinymat", "Objects: wrong_name"))
  l <- tinydisk_loader()
  o <- l$open
  l$open <- function(descriptor, paths, context) {
    stats::setNames(o(descriptor, paths, context), "real")
  }
  data_loader_add(l)
  data_attach("pdsnapmm", lib.loc = lib)
  expect_error(data_get("wrong_name"), class = "projectData_mapping_error")
  cat <- data_catalog()
  expect_equal(c(cat$state, cat$mapping, cat$storage), c("ready", "mismatch", "disk-backed"))
  data_loader_remove("tinydisk::tinymat")
  expect_equal(data_catalog()$storage, "disk-backed")
  data_inspect("pdsnapmm", "real")
  cat <- data_catalog()
  expect_equal(c(cat$object, cat$mapping, cat$storage), c("real", "verified", "disk-backed"))
})

test_that("aliases, explicit access and catalog calls share one opening and snapshot", {
  local_clean_session()
  lib <- local_tinydisk_lib("shared")
  cl <- counting_tinydisk()
  data_loader_add(cl$loader)
  data_attach("pdsnap", lib.loc = lib, suffix = "_s")
  before <- cl$counts$open
  invisible(data_catalog()); invisible(capture.output(print(data_catalog())))
  invisible(data_query("pdsnap", lib.loc = lib, id = "q"))
  expect_equal(cl$counts$open, before)
  expect_equal(cl$counts$open, 0L)

  a <- data_get("shared")
  b <- data_get("shared", package = "pdsnap", lib.loc = lib)
  expect_identical(a, b)
  expect_equal(cl$counts$open, 1L)
  cat <- data_catalog()
  expect_gte(nrow(cat), 1L)
  expect_true(all(cat$storage == "disk-backed" & cat$state == "ready"))
  data_loader_remove("tinydisk::tinymat")
  data_loader_add(memory_variant(cl$counts), replace = TRUE)
  expect_true(all(data_catalog()$storage == "disk-backed"))
  expect_identical(data_get("shared"), a)
  expect_equal(cl$counts$open, 1L)
})
