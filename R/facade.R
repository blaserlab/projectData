# Deferred access uses active bindings. Trade-off versus promises:
# a promise caches permanently but cannot be re-tried cleanly after an
# interrupted evaluation and its failures are sticky in odd ways; an active
# binding re-enters projectData on each access, so it can report ambiguity,
# failure and retry state, and all reads go through the single resource cache.
# Both can be forced by IDE completion or previews; see the IDE reproduction.

plan_bindings <- function(st, res = resolve_names(st)) {
  list(bound = res[res$binding == "bound", , drop = FALSE],
       ambiguous = res[res$binding == "ambiguous", , drop = FALSE])
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

sync_all <- function() sync_facade(resolve_names(.state))

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

#' Attach data packages
#'
#' `data_attach()` is how data become available. Given the names of installed
#' data packages, it inventories each one (reading package metadata only, never
#' data values) and puts its objects, by their own names, in one environment,
#' by default named `"projectData:data"`, placed on the search path at `pos`.
#' The objects are deferred active bindings: each is read the first time you use
#' it, with a message first (see [data_config()]). Nothing is attached unless
#' you call this function. Call it again to attach more packages; they join the
#' same environment. Attachment is all-or-nothing: if any package fails
#' validation (for example a suffix collision), nothing is attached.
#'
#' Attaching registers each package internally (metadata only) and exposes it.
#' A package that [data_get()] already registered without attaching is reused,
#' not inventoried again, and any resource it already opened stays cached. You
#' never need to register anything yourself.
#'
#' An attached package is a *collection* with an id (by default the package
#' name). Use `id` to attach the same package twice, for example two installed
#' versions from different libraries. Attaching an id that is already attached
#' from the same package does nothing, unless the installed package has changed
#' since, which is an error asking for `replace = TRUE`. Registration arguments
#' (`lib.loc`, `suffix`, `include`) for an id that is already attached also need
#' `replace = TRUE`.
#'
#' The installed copy is resolved once, here, from `lib.loc` (default
#' [.libPaths()]); later changes to [.libPaths()] cannot redirect a pending
#' read. If the installed package is replaced in place before an object is
#' opened, opening fails informatively and you re-attach with `replace = TRUE`.
#' Objects you already hold are not closed, but replacing or removing an
#' installation is not covered by any guarantee for them: see the "Lifetime of
#' opened objects" section of [data_detach()].
#'
#' @section Name conflicts:
#' Name resolution is metadata-only. The bare name of an object that exists in
#' several attached collections is bound only if you designated a primary
#' collection ([data_primary()]) or a preference ([data_prefer()]); otherwise it
#' is bound to an error naming the candidates. Attaching still succeeds, and the
#' ambiguity shows in [data_catalog()]. Suffix aliases always resolve and must be
#' unique.
#'
#' @section Masking:
#' Objects in `.GlobalEnv` shadow the attached environment (it sits after
#' `.GlobalEnv`) and are never touched. `on_mask` controls what happens when
#' exposed names also exist elsewhere on the search path: `"warn"` (default),
#' `"error"` (checked before anything changes) or `"ignore"`.
#'
#' @section Selective attachment and sibling objects:
#' `include` limits which objects are exposed (by object or resource name).
#' Opening one object loads its whole resource, so siblings from the same
#' resource are opened and cached together even if not exposed.
#'
#' @section Mapping completeness:
#' The catalog's `origin` column says how a resource was discovered (`index`,
#' `adapter` or `manifest`) and its `mapping` column whether the object names
#' were checked: `recorded` (named by the origin, unchecked), `unknown`,
#' `verified` (opened or inspected, and produced) or `mismatch`. Unknown
#' mappings are never guessed. See [data_catalog()].
#'
#' @section Script datasets (`data/*.R`):
#' Packages that ship non-lazy data scripts are supported. The script is run
#' only when its resource is first opened, by [utils::data()], in a fresh
#' environment whose parent is [baseenv()]. Base functions and `<-` are found;
#' the global workspace and attached packages are not visible. Calls to
#' functions from other packages, including `stats` and `utils`, must be
#' qualified (`stats::rnorm()`, `utils::head()`), otherwise opening fails with
#' "could not find function" and the resource is marked failed (see
#' [data_retry()]). `R CMD INSTALL` evaluates such scripts with default packages
#' attached, so an unqualified call can install cleanly and fail only here.
#'
#' @param packages Names of installed packages to attach.
#' @param lib.loc Library directories searched, in order, for each package.
#' @param id Collection ids (default: the package names).
#' @param suffix `NULL` or one suffix per package (`NA` for none). Also exposes
#'   each object as `<object><suffix>`, e.g. `cds_main.reference`.
#' @param include `NULL`, or a named list (names are collection ids) of
#'   object/resource names to expose. A character vector is accepted for one
#'   package.
#' @param primary `NULL` or the id of the collection whose objects keep bare
#'   names when contested. Equivalent to calling [data_primary()].
#' @param replace Replace collections that are already attached.
#' @param name Search-path name of the environment.
#' @param pos Search-path position.
#' @param on_mask One of `"warn"`, `"error"`, `"ignore"`.
#' @return Invisibly, the names now bound in the environment.
#' @seealso [data_query()] to look at packages without attaching them;
#'   [data_get()] to retrieve one object without attaching.
#' @export
data_attach <- function(packages, lib.loc = NULL, id = packages,
                        suffix = NULL, include = NULL, primary = NULL,
                        replace = FALSE, name = "projectData:data", pos = 2L,
                        on_mask = c("warn", "error", "ignore")) {
  on_mask <- match.arg(on_mask)
  stopifnot(is.character(packages), length(packages) >= 1L)
  id <- as.character(id)
  if (length(id) != length(packages)) pd_abort("`id` must have one element per package.")
  if (!is.null(suffix) && length(suffix) != length(packages)) {
    pd_abort("`suffix` must have one element per package.")
  }
  cand <- snapshot()
  registered <- id %in% names(cand$collections)
  # A registration that data_get() made without attaching may be rebuilt freely
  # (its resource records are reused); only attached ids need `replace`.
  unattached <- registered & !vapply(id, function(i) isTRUE(cand$collections[[i]]$attached), NA)
  existing <- registered & !replace & is.null(lib.loc) & is.null(suffix) & is.null(include)
  for (i in which(existing)) {
    co <- get_collection(id[i], cand)
    if (!identical(co$package, packages[i])) {
      pd_abort("Collection id '", id[i], "' is already ", if (isTRUE(co$attached)) "attached" else "registered", " from package '", co$package,
               "', not '", packages[i], "'. Choose another `id`, or use replace = TRUE.")
    }
    check_installation_unchanged(co)
  }
  if (any(!existing)) {
    cand <- add_collections(cand, packages[!existing], lib.loc, id[!existing],
                            if (is.null(suffix)) NULL else suffix[!existing],
                            include, replace | unattached[!existing])
  }
  for (i in id) cand$collections[[i]]$attached <- TRUE
  if (!is.null(primary)) cand$primary <- primary
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

#' Detach data packages
#'
#' Removes collections (default: all) from the attached environment and forgets
#' them, along with any preferences and primary designation that refer to them.
#' This covers collections that are only registered (for example by
#' `data_get(package = )`) as well: their internal registration and cached
#' references to opened resources are dropped, so the next access registers and
#' opens them afresh. When no attached collection remains, the environment is
#' removed from the search path. Only the environment projectData created is
#' touched.
#'
#' @section Lifetime of opened objects:
#' Detaching and replacing an installed package are different events.
#'
#' * Detaching (and any registration cleanup) never deletes, moves or closes
#'   backing files, and does not modify objects you hold. An object you kept in
#'   a variable can therefore keep working after detachment, because the files it
#'   reads from are still there. Work directories created for loaders stay in the
#'   session temporary directory.
#' * Detaching does **not** protect objects from the installation changing.
#'   Fully materialised in-memory values generally stay usable. A disk-backed
#'   object that references files inside an installed package depends on those
#'   files remaining available and compatible; if the package is replaced or
#'   removed it may stop working, or read different data.
#' * An object that references an adapter-managed working copy is independent of
#'   the installation only if the adapter restored every file reference it needs
#'   to the working copy, and only for as long as that copy exists (by default,
#'   the session temporary directory).
#'
#' Which case applies to an object is decided by its loader; see
#' [data_loader()].
#'
#' @param collections Registered collection ids; `NULL` detaches everything.
#' @return Invisibly, the detached ids.
#' @export
data_detach <- function(collections = NULL) {
  cand <- snapshot()
  env <- cand$facade$env
  ids <- collections %||% names(cand$collections)
  for (i in ids) get_collection(i, cand)
  cand$collections[ids] <- NULL
  cand$preferences <- cand$preferences[!cand$preferences %in% ids]
  if (cand$primary %in% ids) cand$primary <- NA_character_
  validate_state(cand)
  commit_state(cand)
  if (!length(attached_collections(.state)) && !is.na(facade_pos(env))) {
    clear_env(env)
    do.call("detach", list(name = .state$facade$name, character.only = TRUE))
    .state$facade$env <- NULL
  }
  invisible(ids)
}
