# Provenance (origin), mapping verification, loader status and messages.

provenance_fixture <- function() {
  lib <- validation_fixtures()
  fixture_package(lib, "pdprov", "0.1.0", lazy = FALSE, datasets = list(),
    files = list("extdata/vres.txt" = function(path) writeLines("x", path)),
    manifest = c("ManifestVersion: 1", "",
                 "Resource: vmiss", "Loader: pdvaladp::nothere", "Source: extdata/vres.txt",
                 "Objects: vmiss_obj", "AdapterPackage: pdvaladp", "",
                 "Resource: vbad", "Loader: pdvaladp::a", "Source: extdata/vres.txt",
                 "Objects: vbad_obj", "AdapterPackage: pdvaladp", "AdapterExport: loaders_malformed", "",
                 "Resource: vnone", "Loader: pdvaladp::a", "Source: extdata/vres.txt",
                 "Objects: vnone_obj", "AdapterPackage: pdvaladp", "AdapterExport: no_such_export"))
  lib
}

test_that("origin separates standard-index, adapter and manifest discovery from mapping", {
  local_clean_session()
  fx <- shared_fixtures()
  n <- local_open_counter()
  data_loader_add(tinydisk_loader())
  data_attach("pdref", lib.loc = fx$lib)
  cat <- data_catalog()
  row <- function(obj) cat[cat$object %in% obj, ]
  expect_equal(row("m1")[c("origin", "mapping")] |> unlist() |> unname(), c("index", "recorded"))
  expect_equal(row("mat")[c("origin", "mapping")] |> unlist() |> unname(), c("adapter", "recorded"))
  expect_equal(row("ref_rds")[c("origin", "mapping")] |> unlist() |> unname(), c("manifest", "recorded"))
  expect_equal(row("mat")$loader, "tinydisk::tinymat")
  expect_equal(cat$origin[cat$resource == "undoc"], "index")
  expect_length(n$log, 0)
})

test_that("opening or inspecting changes mapping but never origin", {
  local_clean_session()
  fx <- shared_fixtures()
  data_loader_add(tinydisk_loader())
  data_attach("pdref", lib.loc = fx$lib)
  before <- data_catalog()
  data_get("ref_rds"); data_get("mat")
  expect_warning(data_inspect("pdref"), class = "projectData_inspection")
  after <- data_catalog()
  expect_equal(after$mapping[after$object %in% c("ref_rds", "mat", "hidden")], rep("verified", 3))
  expect_equal(after$origin[after$object %in% "hidden"], "index")
  same <- before$object[!is.na(before$object)]
  expect_equal(after$origin[match(same, after$object)], before$origin[!is.na(before$object)])
  expect_equal(after$origin[after$object %in% "ref_rds"], "manifest")
  expect_equal(after$origin[after$object %in% "mat"], "adapter")
})

test_that("an installed but unresolved adapter is not reported as a ready loader", {
  local_clean_session()
  lib <- provenance_fixture()
  local_adapter_lib(lib)
  log <- local_event_log()
  data_query("pdvalpkg", lib.loc = lib)
  expect_false("pdvaladp" %in% loadedNamespaces())
  data_attach("pdvalpkg", lib.loc = lib)
  cat <- data_catalog()
  v <- cat[cat$object %in% "vres_obj", ]
  expect_equal(v$loader_status, "unresolved")
  expect_equal(v$state, "unopened")
  expect_equal(v$storage, "unknown")
  expect_equal(cat$loader_status[cat$object %in% "plain_obj"], "registered")
  expect_false("pdvaladp" %in% loadedNamespaces())
  expect_false("pdvaladp::v" %in% data_loaders()$id)
  expect_length(log$events, 0)
})

test_that("a missing adapter package is unavailable, not unresolved", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib)
  expect_equal(data_catalog()$loader_status[data_catalog()$object %in% "fancy_obj"], "unavailable")
})

test_that("resolution gives targeted diagnostics and records the failure", {
  local_clean_session()
  lib <- provenance_fixture()
  local_adapter_lib(lib)
  log <- local_event_log()
  data_attach("pdprov", lib.loc = lib)
  status <- function(o) {
    cat <- data_catalog()
    unlist(cat[cat$object %in% o, c("loader_status", "state")], use.names = FALSE)
  }

  expect_error(data_get("vmiss_obj"), "does not provide loader 'pdvaladp::nothere'.*provides: pdvaladp::v",
               class = "projectData_loader_unavailable")
  expect_error(data_get("vbad_obj"), "pdvaladp::loaders_malformed.*invalid declaration.*`open` must be a function",
               class = "projectData_loader_unavailable")
  expect_error(data_get("vnone_obj"), "does not export 'no_such_export'",
               class = "projectData_loader_unavailable")
  expect_equal(status("vmiss_obj"), c("adapter-error", "unavailable-loader"))
  expect_equal(status("vbad_obj"), c("adapter-error", "unavailable-loader"))
  expect_equal(status("vnone_obj"), c("adapter-error", "unavailable-loader"))
  expect_false(any(c("open", "validate") %in% log$what()))
  # Failed declarations register nothing.
  expect_false(any(grepl("^pdvaladp::(a|bad)$", data_loaders()$id)))
})

test_that("a valid registered loader is reported as registered, separately from opening", {
  local_clean_session()
  lib <- validation_fixtures()
  local_adapter_lib(lib)
  log <- local_event_log()
  data_attach("pdvalpkg", lib.loc = lib)
  expect_equal(data_catalog()$loader_status[data_catalog()$object %in% "vres_obj"], "unresolved")
  expect_identical(data_get("vres_obj"), "opened")
  v <- data_catalog()[data_catalog()$object %in% "vres_obj", ]
  expect_equal(c(v$loader_status, v$state, v$mapping), c("registered", "ready", "verified"))

  # A registered loader whose validation rejects the resource is still registered.
  data_detach()
  data_attach("pdvalpkg", lib.loc = lib)
  withr::local_options(pdtest.reject = TRUE)
  expect_error(data_get("vres_obj"), class = "projectData_validation_failed")
  v <- data_catalog()[data_catalog()$object %in% "vres_obj", ]
  expect_equal(c(v$loader_status, v$state), c("registered", "failed"))
})

test_that("catalog and query operations do not invoke resource readers or adapters", {
  local_clean_session()
  lib <- provenance_fixture()
  fx <- shared_fixtures()
  local_adapter_lib(lib)
  n <- local_open_counter()
  log <- local_event_log()
  data_loader_add(tinydisk_loader())
  data_query("pdprov", lib.loc = lib)
  data_attach("pdref", lib.loc = fx$lib)
  data_attach("pdprov", lib.loc = lib)
  data_catalog(); data_info("mat"); print(data_catalog())
  expect_length(n$log, 0)
  expect_equal(log$what(), character())
  expect_false("pdvaladp" %in% loadedNamespaces())
})

test_that("opening messages make no promise about reader behavior", {
  local_clean_session()
  fx <- shared_fixtures()
  data_attach("pdref", lib.loc = fx$lib)
  data_config(messages = TRUE)
  msg <- testthat::capture_messages(data_get("only_ref"))
  expect_match(msg, "opening resource 'only'")
  expect_no_match(msg, "download", ignore.case = TRUE)
  expect_no_match(msg, "nothing is")
})
