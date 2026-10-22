#' projectData: Deferred Access to Installed Data Collections
#'
#' Registers installed data packages ("collections") without reading their data,
#' exposes objects through deferred bindings, resolves name conflicts through
#' explicit choices, and records provenance. See `vignette("projectData")`.
#'
#' Related tools: `conflicted` resolves conflicts between package exports, and
#' base `library(include.only = )` / `use()` select exports. Those work with
#' exported data, but do not inventory collections, defer access outside the
#' constraints of lazy data, restore format-specific resources, or record
#' provenance; that is what projectData adds. It is not an out-of-memory
#' computation engine and does not guarantee reproducibility.
#'
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  reset_state()
}

.onUnload <- function(libpath) {
  env <- .state$facade$env
  if (!is.null(env) && !is.na(facade_pos(env))) {
    clear_env(env)
    try(do.call("detach", list(name = .state$facade$name, character.only = TRUE)), silent = TRUE)
  }
}
