source(system.file("examples", "fixtures.R", package = "projectData"), local = TRUE)
source(system.file("examples", "tinydisk.R", package = "projectData"), local = TRUE)

fixture_cache <- new.env()

# Built once per test run; all offline, in temporary libraries.
shared_fixtures <- function() {
  if (!is.null(fixture_cache$lib)) return(fixture_cache)
  lib <- fixture_library()
  lib2 <- fixture_library()
  refobj <- function(path) saveRDS(list(value = "rds-object"), path)
  fixture_package(lib, "pdmain", "0.1.0", lazy = TRUE, datasets = list(
    cds_main = list(cds_main = "main-v1"),
    meta = list(meta_a = 1, meta_b = 2),
    pair = list(pair_x = "x", pair_y = "y")
  ))
  fixture_package(lib2, "pdmain", "0.2.0", lazy = TRUE, datasets = list(
    cds_main = list(cds_main = "main-v2")
  ))
  fixture_package(lib, "pdref", "0.1.0", lazy = FALSE, datasets = list(
    cds_main = list(cds_main = "ref"),
    only = list(only_ref = 3),
    multi = list(m1 = "m1", m2 = "m2"),
    undoc = list(hidden = 5)
  ), drop_index = "undoc",
  files = list(
    "extdata/refobj.rds" = refobj,
    "extdata/mat.tinymat" = function(path) tinymat_write(matrix(as.double(1:12), 3, 4), path)
  ),
  manifest = c(
    "ManifestVersion: 1", "",
    "Resource: refobj", "Loader: projectData::rds", "Source: extdata/refobj.rds",
    "Objects: ref_rds", "Title: A reference RDS object", "RecordedClass: list", "",
    "Resource: fancy", "Loader: fancyfmt::open", "Source: extdata/refobj.rds",
    "Objects: fancy_obj", "AdapterPackage: notinstalledfancypkg"
  ))
  fixture_package(lib, "pdmisc", "0.1.0", lazy = FALSE,
    datasets = list(plain = list(plain_obj = 1)),
    files = list("extdata/refobj.rds" = refobj),
    manifest = c(
      "ManifestVersion: 1", "",
      "Resource: flaky", "Loader: test::flaky", "Source: extdata/refobj.rds", "Objects: flaky_obj", "",
      "Resource: reenter", "Loader: test::reenter", "Source: extdata/refobj.rds", "Objects: reenter_obj"
    ))
  fixture_cache$lib <- lib
  fixture_cache$lib2 <- lib2
  fixture_cache
}

# Restores registry, facade, loaders, config, libPaths and options on exit.
local_clean_session <- function(env = parent.frame()) {
  old_libs <- .libPaths()
  old_cfg <- projectData:::.state$config
  withr::defer({
    suppressWarnings(try(projectData::data_detach(), silent = TRUE))
    for (id in setdiff(projectData::data_loaders()$id, projectData::data_loaders()$id[projectData::data_loaders()$builtin])) {
      projectData::data_loader_remove(id)
    }
    projectData::data_primary(NULL)
    assign("config", old_cfg, envir = projectData:::.state)
    .libPaths(old_libs)
  }, envir = env)
  projectData::data_config(messages = FALSE, on_open = NULL)
  invisible()
}

# Counts loader invocations without relying on memory heuristics.
local_open_counter <- function() {
  counter <- new.env()
  counter$log <- character()
  projectData::data_config(on_open = function(info) {
    counter$log <- c(counter$log, paste(info$collection, info$resource, sep = "/"))
  })
  counter
}
