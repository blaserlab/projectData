# A real installed adapter package, so loaders are resolved by namespace as in
# production. Its callbacks report to the function in option `pdtest.log`.
adapter_source <- c(
  'pdlog <- function(what, ...) {',
  '  f <- getOption("pdtest.log")',
  '  if (is.function(f)) f(what, ...)',
  '  invisible()',
  '}',
  'mk <- function(id) {',
  '  projectData::data_loader(id,',
  '    discover = function(root, context) { pdlog("discover"); NULL },',
  '    validate = function(descriptor, root, context) {',
  '      pdlog("validate", id = id, context = context, root = root, descriptor = descriptor)',
  '      if (isTRUE(getOption("pdtest.reject"))) return("rejected by test")',
  '      file.exists(file.path(root, descriptor$source)) || "source file missing"',
  '    },',
  '    open = function(descriptor, paths, context) {',
  '      pdlog("open", id = id, context = context, paths = paths)',
  '      stats::setNames(list("opened"), descriptor$objects)',
  '    })',
  '}',
  'projectData_loaders <- function() list(mk("pdvaladp::v"), mk("pdvaladp::w"))',
  'loaders_single <- function() mk("pdvaladp::v")',
  'loaders_dup <- function() list(mk("pdvaladp::a"), mk("pdvaladp::b"), mk("pdvaladp::a"))',
  'loaders_malformed <- function() {',
  '  bad <- mk("pdvaladp::bad"); bad$open <- "not a function"',
  '  list(mk("pdvaladp::a"), bad)',
  '}',
  'loaders_nonloader <- function() list(mk("pdvaladp::a"), list(id = "pdvaladp::b"))',
  'loaders_none <- function() list()',
  'loaders_collide <- function() list(mk("pdvaladp::fresh"), mk("pdvaladp::v"), mk("pdvaladp::w"))'
)

install_adapter_package <- function(lib, name = "pdvaladp") {
  src <- file.path(tempfile("pd-src-"), name)
  dir.create(file.path(src, "R"), recursive = TRUE)
  writeLines(c(paste("Package:", name), "Title: Fixture adapter", "Version: 0.1.0",
               "Description: Synthetic fixture.", "License: GPL-3", "Author: fixture"),
             file.path(src, "DESCRIPTION"))
  writeLines("export(projectData_loaders, loaders_single, loaders_dup, loaders_malformed,\n  loaders_nonloader, loaders_none, loaders_collide)",
             file.path(src, "NAMESPACE"))
  writeLines(adapter_source, file.path(src, "R", "adapter.R"))
  system2(file.path(R.home("bin"), "R"),
          c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(src)),
          stdout = TRUE, stderr = TRUE)
  invisible(lib)
}

# Data package with one manifest resource handled by pdvaladp::v.
install_validation_fixtures <- function(lib) {
  fixture_package(lib, "pdvalpkg", "0.3.0", lazy = FALSE,
    datasets = list(plain = list(plain_obj = 1)),
    files = list("extdata/vres.txt" = function(path) writeLines("x", path)),
    manifest = c("ManifestVersion: 1", "",
                 "Resource: vres", "Loader: pdvaladp::v", "Source: extdata/vres.txt",
                 "Objects: vres_obj", "AdapterPackage: pdvaladp"))
  install_adapter_package(lib)
}

# Event log for the adapter callbacks; restored with the calling test.
local_event_log <- function(env = parent.frame()) {
  log <- new.env()
  log$events <- list()
  withr::local_options(pdtest.log = function(what, ...) {
    log$events[[length(log$events) + 1L]] <- c(list(what = what), list(...))
  }, pdtest.reject = NULL, .local_envir = env)
  log$what <- function() vapply(log$events, function(e) e$what, "")
  log
}

validation_fixtures <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- fixture_library()
      install_validation_fixtures(cache)
    }
    cache
  }
})

# Makes the adapter findable by namespace and unloads it afterwards.
local_adapter_lib <- function(lib, env = parent.frame()) {
  old <- .libPaths()
  .libPaths(c(lib, old))
  withr::defer({
    if ("pdvaladp" %in% loadedNamespaces()) unloadNamespace("pdvaladp")
    .libPaths(old)
  }, envir = env)
}
