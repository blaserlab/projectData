# Installation lifecycle: detection of a changed installation and replacement.
# Fixture packages are installed into temporary libraries; reads are counted
# with `local_open_counter()`. Nothing relies on timing.

install_pkg <- function(lib, name, version = "0.1.0", values = list(a = "old", b = "bee")) {
  unlink(file.path(lib, name), recursive = TRUE)
  datasets <- stats::setNames(lapply(names(values), function(n) stats::setNames(list(values[[n]]), n)),
                              names(values))
  fixture_package(lib, name, version, lazy = FALSE, datasets = datasets)
}

facade_names <- function() ls(as.environment("projectData:data"), all.names = TRUE)

test_that("re-attaching an unchanged installation is a no-op that keeps the cache", {
  local_clean_session()
  lib <- fixture_library()
  install_pkg(lib, "pdr1")
  n <- local_open_counter()
  data_attach("pdr1", lib.loc = lib)
  expect_identical(data_get("a", collection = "pdr1"), "old")
  expect_length(n$log, 1)
  before <- projectData:::.state$collections
  data_attach("pdr1")
  data_attach("pdr1", id = "pdr1")
  expect_identical(projectData:::.state$collections, before)
  expect_identical(data_get("a", collection = "pdr1"), "old")
  expect_length(n$log, 1)
})

test_that("a newer version at the same root makes ordinary attachment fail informatively", {
  local_clean_session()
  lib <- fixture_library()
  install_pkg(lib, "pdr2")
  data_attach("pdr2", lib.loc = lib)
  install_pkg(lib, "pdr2", "0.2.0", list(a = "new", b = "bee"))
  before <- projectData:::.state$collections
  err <- expect_error(data_attach("pdr2"), class = "projectData_stale_installation")
  msg <- conditionMessage(err)
  expect_match(msg, "collection 'pdr2'", fixed = TRUE)
  expect_mentions_path(msg, file.path(lib, "pdr2"))
  expect_match(msg, "registered version 0.1.0", fixed = TRUE)
  expect_match(msg, "on disk now 0.2.0", fixed = TRUE)
  expect_match(msg, "replace = TRUE", fixed = TRUE)
  expect_identical(projectData:::.state$collections, before)
  expect_equal(nrow(data_collections()), 1)
  expect_equal(data_collections()$version, "0.1.0")
  # the same check guards opening
  expect_error(data_get("a", collection = "pdr2"), class = "projectData_stale_installation")
})

test_that("a same-version reinstall and a removal are detected", {
  local_clean_session()
  lib <- fixture_library()
  install_pkg(lib, "pdr3")
  data_attach("pdr3", lib.loc = lib)
  install_pkg(lib, "pdr3", "0.1.0", list(a = "a much longer replacement value", b = "bee"))
  err <- expect_error(data_attach("pdr3"), class = "projectData_stale_installation")
  expect_match(conditionMessage(err), "on disk now 0.1.0", fixed = TRUE)
  unlink(file.path(lib, "pdr3"), recursive = TRUE)
  err <- expect_error(data_attach("pdr3"), class = "projectData_stale_installation")
  expect_match(conditionMessage(err), "not available", fixed = TRUE)
  expect_match(conditionMessage(err), "removed", fixed = TRUE)
})

test_that("a library link repointed to another installation is detected (resolved root unchanged)", {
  skip_on_os("windows")
  local_clean_session()
  lib <- fixture_library()
  store1 <- file.path(lib, "store1"); store2 <- file.path(lib, "store2")
  dir.create(store1); dir.create(store2)
  install_pkg(store1, "pdr4")
  install_pkg(store2, "pdr4", "0.2.0", list(a = "new", b = "bee"))
  link <- file.path(lib, "pdr4")
  skip_if_not(file.symlink(file.path(store1, "pdr4"), link))
  data_attach("pdr4", lib.loc = lib)
  expect_match(data_collections()$library, "store1$")
  # the old physical files are untouched and still look unchanged
  unlink(link)
  file.symlink(file.path(store2, "pdr4"), link)
  err <- expect_error(data_attach("pdr4"), class = "projectData_stale_installation")
  expect_match(conditionMessage(err), "now resolves to", fixed = TRUE)
  expect_match(conditionMessage(err), "on disk now 0.2.0", fixed = TRUE)
  expect_error(data_get("a", collection = "pdr4"), class = "projectData_stale_installation")
  data_attach("pdr4", replace = TRUE)
  expect_identical(data_get("a", collection = "pdr4"), "new")
})

test_that("replace = TRUE exposes new values and discards old caches", {
  local_clean_session()
  lib <- fixture_library()
  install_pkg(lib, "pdr5")
  n <- local_open_counter()
  data_attach("pdr5", lib.loc = lib)
  held <- data_get("a", collection = "pdr5")
  expect_identical(held, "old")
  expect_length(n$log, 1)
  descriptors <- lapply(projectData:::.state$collections$pdr5$resources, `[[`, "descriptor")
  install_pkg(lib, "pdr5", "0.1.0", list(a = "new", b = "newbee"))
  expect_error(data_attach("pdr5"), class = "projectData_stale_installation")
  data_attach("pdr5", replace = TRUE)
  expect_identical(lapply(projectData:::.state$collections$pdr5$resources, `[[`, "descriptor"),
                   descriptors)
  expect_identical(data_catalog("pdr5")$state, c("unopened", "unopened"))
  expect_identical(data_get("a", collection = "pdr5"), "new")
  expect_length(n$log, 2)
  # attached (bare name) access also uses the new records, and shares the cache
  expect_identical(get("a", envir = as.environment("projectData:data")), "new")
  expect_identical(get("b", envir = as.environment("projectData:data")), "newbee")
  expect_length(n$log, 3)
  # objects held elsewhere keep the old value
  expect_identical(held, "old")
})

test_that("replacement preserves omitted installation and exposure settings", {
  local_clean_session()
  lib <- fixture_library()
  other <- fixture_library()
  install_pkg(lib, "pdr6")
  # a different, later installation of the same package elsewhere on .libPaths()
  install_pkg(other, "pdr6", "9.9.9", list(a = "elsewhere", b = "elsewhere"))
  data_attach("pdr6", lib.loc = lib, id = "main", suffix = ".s", include = "a", primary = "main")
  install_pkg(lib, "pdr6", "0.2.0", list(a = "new", b = "bee"))
  .libPaths(c(other, .libPaths()))
  data_attach("pdr6", id = "main", replace = TRUE)
  co <- data_collections()
  expect_equal(co$version, "0.2.0")
  expect_equal(co$suffix, ".s")
  expect_true(projectData:::same_path(co$library, lib))
  expect_equal(data_primary(), "main")
  expect_setequal(facade_names(), c("a", "a.s"))
  expect_identical(data_get("a", collection = "main"), "new")
  # explicit arguments take precedence
  data_attach("pdr6", id = "main", suffix = ".t", include = c("a", "b"), replace = TRUE)
  expect_setequal(facade_names(), c("a", "b", "a.t", "b.t"))
  data_attach("pdr6", id = "main", suffix = NA, replace = TRUE)
  expect_setequal(facade_names(), c("a", "b"))
  data_attach("pdr6", id = "main", lib.loc = other, replace = TRUE)
  expect_equal(data_collections()$version, "9.9.9")
})

test_that("a failed replacement leaves registration, bindings and preferences unchanged", {
  local_clean_session()
  lib <- fixture_library()
  install_pkg(lib, "pdr7")
  install_pkg(lib, "pdr7b", values = list(a = "rival"))
  data_attach(c("pdr7", "pdr7b"), lib.loc = lib, primary = "pdr7")
  data_prefer("a", "pdr7")
  before <- list(coll = projectData:::.state$collections, prefs = data_preferences(),
                 primary = data_primary(), names = facade_names(), search = search())
  # the new installation no longer provides `a`: the preference would be invalid
  install_pkg(lib, "pdr7", "0.2.0", list(z = "zed", b = "bee"))
  err <- expect_error(data_attach("pdr7", replace = TRUE), class = "projectData_replace_failed")
  expect_match(conditionMessage(err), "does not restore", fixed = TRUE)
  expect_match(conditionMessage(err), "Preference for 'a'", fixed = TRUE)
  expect_identical(projectData:::.state$collections, before$coll)
  expect_identical(data_preferences(), before$prefs)
  expect_identical(data_primary(), before$primary)
  expect_identical(facade_names(), before$names)
  expect_identical(search(), before$search)
  # the package disappearing from the library fails the same way
  unlink(file.path(lib, "pdr7"), recursive = TRUE)
  expect_error(data_attach("pdr7", replace = TRUE), class = "projectData_not_installed")
  expect_identical(projectData:::.state$collections, before$coll)
})

test_that("repeated replacement keeps one collection and one search-path entry", {
  local_clean_session()
  lib <- fixture_library()
  install_pkg(lib, "pdr8")
  data_attach("pdr8", lib.loc = lib)
  for (v in c("0.2.0", "0.3.0", "0.3.0")) {
    install_pkg(lib, "pdr8", v, list(a = v, b = "bee"))
    data_attach("pdr8", replace = TRUE)
  }
  expect_equal(nrow(data_collections()), 1)
  expect_equal(sum(search() == "projectData:data"), 1)
  expect_equal(names(projectData:::.state$collections), "pdr8")
  expect_identical(data_get("a", collection = "pdr8"), "0.3.0")
  expect_setequal(facade_names(), c("a", "b"))
})

test_that("changing .libPaths() alone does not redirect an existing collection", {
  local_clean_session()
  lib_a <- fixture_library()
  lib_b <- fixture_library()
  install_pkg(lib_a, "pdr9")
  install_pkg(lib_b, "pdr9", "0.2.0", list(a = "from-b", b = "bee"))
  data_attach("pdr9", lib.loc = lib_a)
  .libPaths(c(lib_b, .libPaths()))
  data_attach("pdr9")
  expect_equal(data_collections()$version, "0.1.0")
  expect_identical(data_get("a", package = "pdr9"), "old")
  # an explicit selection of the other installation needs replace = TRUE
  before <- projectData:::.state$collections
  err <- expect_error(data_attach("pdr9", lib.loc = lib_b), class = "projectData_installation_conflict")
  expect_match(conditionMessage(err), "replace = TRUE", fixed = TRUE)
  expect_identical(projectData:::.state$collections, before)
  data_attach("pdr9", lib.loc = lib_b, replace = TRUE)
  expect_identical(data_get("a", collection = "pdr9"), "from-b")
})

test_that("distinct ids keep separate installed versions side by side", {
  local_clean_session()
  lib_a <- fixture_library()
  lib_b <- fixture_library()
  install_pkg(lib_a, "pdr10")
  install_pkg(lib_b, "pdr10", "0.2.0", list(a = "from-b", b = "bee"))
  data_attach("pdr10", lib.loc = lib_a, id = "v1")
  data_attach("pdr10", lib.loc = lib_b, id = "v2")
  expect_equal(data_collections()$version, c("0.1.0", "0.2.0"))
  expect_identical(data_get("a", collection = "v1"), "old")
  expect_identical(data_get("a", collection = "v2"), "from-b")
  data_attach("pdr10", id = "v2")
  expect_equal(nrow(data_collections()), 2)
  install_pkg(lib_a, "pdr10", "0.3.0", list(a = "newest", b = "bee"))
  expect_error(data_attach("pdr10", id = "v1"), class = "projectData_stale_installation")
  expect_identical(data_get("a", collection = "v2"), "from-b")
})
