# Deferred access uses active bindings. Trade-off versus promises:
# a promise caches permanently but cannot be re-tried cleanly after an
# interrupted evaluation and its failures are sticky in odd ways; an active
# binding re-enters projectData on each access, so it can report ambiguity,
# failure and retry state, and all reads go through the single resource cache.
# Both can be forced by IDE completion or previews; see the IDE reproduction.

plan_bindings <- function(st, res = resolve_names(st)) {
  att <- st$attached
  bound <- res[res$binding == "bound" & res$collection %in% att, , drop = FALSE]
  amb_names <- unique(res$name[res$binding == "ambiguous" & res$collection %in% att])
  amb <- res[res$binding == "ambiguous" & res$name %in% amb_names, , drop = FALSE]
  list(bound = bound, ambiguous = amb)
}

planned_names <- function(st, res = resolve_names(st)) {
  p <- plan_bindings(st, res)
  unique(c(p$bound$name, p$ambiguous$name))
}

object_binding <- function(id, object, name) {
  force(id); force(object); force(name)
  function(value) {
    if (!missing(value)) {
      pd_abort("'", name, "' is a read-only projectData binding.")
    }
    get_object(id, object)
  }
}

ambiguous_binding <- function(name, msg) {
  force(msg)
  function(value) {
    if (!missing(value)) pd_abort("'", name, "' is a read-only projectData binding.")
    pd_abort(msg, class = "projectData_ambiguous")
  }
}

clear_env <- function(env) {
  rm(list = ls(env, all.names = TRUE), envir = env)
}

sync_all <- function() {
  res <- resolve_names(.state)
  for (co in .state$collections) {
    clear_env(co$env)
    o <- co$objects[co$objects$exposed & !is.na(co$objects$object), , drop = FALSE]
    for (nm in o$object) {
      makeActiveBinding(nm, object_binding(co$id, nm, nm), co$env)
    }
  }
  sync_facade(res)
}

facade_pos <- function(env) {
  if (is.null(env)) return(NA_integer_)
  for (i in seq_along(search())) if (identical(as.environment(i), env)) return(i)
  NA_integer_
}

sync_facade <- function(res = resolve_names(.state)) {
  env <- .state$facade$env
  if (is.na(facade_pos(env))) return(invisible())
  clear_env(env)
  p <- plan_bindings(.state, res)
  for (i in seq_len(nrow(p$bound))) {
    makeActiveBinding(p$bound$name[i],
                      object_binding(p$bound$collection[i], p$bound$object[i], p$bound$name[i]), env)
  }
  for (nm in unique(p$ambiguous$name)) {
    makeActiveBinding(nm, ambiguous_binding(nm, ambiguity_message(nm, res)), env)
  }
  invisible()
}

# Masking --------------------------------------------------------------------

find_masks <- function(names, facade_env) {
  out <- list()
  for (i in seq_along(search())) {
    e <- as.environment(i)
    if (!is.null(facade_env) && identical(e, facade_env)) next
    hit <- intersect(names, ls(e, all.names = TRUE))
    if (length(hit)) out[[length(out) + 1L]] <- data.frame(name = hit, where = search()[i],
                                                           stringsAsFactors = FALSE)
  }
  rbind_list(out, data.frame(name = character(), where = character(), stringsAsFactors = FALSE))
}

mask_message <- function(m) {
  glob <- m$name[m$where == ".GlobalEnv"]
  other <- m[m$where != ".GlobalEnv", , drop = FALSE]
  paste0(
    if (length(glob)) paste0(
      "Object(s) in .GlobalEnv will shadow the projectData facade: ", quote_names(glob),
      ". projectData never overwrites global objects; remove them or use data_get(). "),
    if (nrow(other)) paste0(
      "Facade bindings mask objects elsewhere on the search path: ",
      paste0("'", other$name, "' (", other$where, ")", collapse = ", "), ".")
  )
}

#' Report names masked by, or shadowing, the attached facade
#'
#' Checks only names, never values. Objects in `.GlobalEnv` take precedence over
#' the facade and are never overwritten.
#'
#' @return A data frame with `name` and `where` (the other search-path entry).
#' @export
data_masking <- function() {
  env <- .state$facade$env
  if (is.na(facade_pos(env))) return(find_masks(character(), NULL))
  find_masks(planned_names(.state), env)
}

#' Attach and detach the optional facade
#'
#' `data_attach()` places one environment, by default named
#' `"projectData:data"`, on the search path at `pos` and exposes bare names
#' for the selected registered collections as active bindings. Nothing is
#' attached unless you call it. Registering a collection later does not add it
#' to the facade; call `data_attach()` again.
#'
#' Name resolution is metadata-only. The bare name of an object that exists in
#' several collections is bound only if you designated a primary collection
#' ([data_primary()]) or a preference ([data_prefer()]); otherwise it is bound
#' to an error that names the candidates. Suffix aliases always resolve.
#'
#' Objects in `.GlobalEnv` shadow the facade (it sits after `.GlobalEnv`) and
#' are never touched. `on_mask` controls what happens when exposed names also
#' exist elsewhere on the search path: `"warn"` (default), `"error"` (checked
#' before anything changes) or `"ignore"`.
#'
#' `data_detach()` removes collections from the facade, or the whole facade when
#' `collections` is `NULL`. It only removes the environment projectData
#' created. Objects already returned to you, and the backing files they use,
#' are unaffected.
#'
#' @param collections Collection ids; `NULL` means all registered collections
#'   (`data_attach`) or the whole facade (`data_detach`).
#' @param name Search-path name of the facade.
#' @param pos Search-path position.
#' @param on_mask One of `"warn"`, `"error"`, `"ignore"`.
#' @return Invisibly, the names bound in the facade.
#' @export
data_attach <- function(collections = NULL, name = "projectData:data", pos = 2L,
                        on_mask = c("warn", "error", "ignore")) {
  on_mask <- match.arg(on_mask)
  cand <- snapshot()
  ids <- collections %||% names(cand$collections)
  for (id in ids) get_collection(id)
  f <- cand$facade
  if (is.na(facade_pos(f$env))) {
    if (name %in% search()) {
      pd_abort("A search-path entry named '", name, "' already exists and is not owned by projectData.")
    }
    f$env <- NULL
    f$name <- name
    f$pos <- as.integer(pos)
  }
  f$on_mask <- on_mask
  cand$facade <- f
  cand$attached <- union(cand$attached, ids)

  validate_state(cand)
  if (is.na(facade_pos(f$env))) {
    do.call("attach", list(NULL, pos = f$pos, name = f$name, warn.conflicts = FALSE))
    cand$facade$env <- as.environment(f$name)
  }
  commit_state(cand)
  nm <- planned_names(.state)
  if (on_mask == "warn") {
    m <- find_masks(nm, .state$facade$env)
    if (nrow(m)) pd_warn(mask_message(m), class = "projectData_masking")
  }
  invisible(nm)
}

#' @rdname data_attach
#' @export
data_detach <- function(collections = NULL) {
  cand <- snapshot()
  env <- cand$facade$env
  if (is.null(collections)) {
    cand$attached <- character()
  } else {
    cand$attached <- setdiff(cand$attached, collections)
  }
  commit_state(cand)
  if (!length(.state$attached) && !is.na(facade_pos(env))) {
    clear_env(env)
    do.call("detach", list(name = .state$facade$name, character.only = TRUE))
    .state$facade$env <- NULL
  }
  invisible()
}
