# Session registry. Populated in .onLoad(); loading the package registers
# nothing but the built-in loaders and never touches user data or options.
.state <- new.env(parent = emptyenv())

state_fields <- c("collections", "preferences", "primary", "facade")

reset_state <- function() {
  .state$collections <- list()
  .state$preferences <- character()
  .state$primary <- NA_character_
  .state$facade <- list(name = "projectData:data", pos = 2L,
                        on_mask = "warn", env = NULL)
  .state$loaders <- list()
  .state$config <- list(messages = TRUE, on_open = NULL)
  .state$work_counter <- 0L
  register_builtin_loaders()
  invisible()
}

snapshot <- function() mget(state_fields, envir = .state)

assign_state <- function(st) {
  list2env(st[state_fields], envir = .state)
  invisible()
}

# All mutations build a candidate, validate it, and only then commit.
commit_state <- function(cand) {
  old <- snapshot()
  assign_state(cand)
  tryCatch(
    sync_all(),
    error = function(e) {
      assign_state(old)
      try(sync_all(), silent = TRUE)
      stop(e)
    }
  )
  invisible()
}

get_collection <- function(id, st = .state) {
  co <- st$collections[[id]]
  if (is.null(co)) {
    pd_abort("No attached collection named '", id, "'. Attached: ",
             if (length(st$collections)) quote_names(names(st$collections)) else "none",
             ". Use data_attach() first.", class = "projectData_unknown_collection")
  }
  co
}

#' Configure projectData session behavior
#'
#' With no arguments, `data_config()` just returns the current settings.
#'
#' @param messages Logical (default `TRUE`). Emit a message the first time a
#'   resource is opened, before any data are read. It names the objects, the
#'   collection and loader, gives the on-disk size of a single source file when
#'   there is one, and states that local installed files are used (nothing is
#'   downloaded). Turn it off with `data_config(messages = FALSE)`; the setting
#'   lasts for the session.
#' @param on_open Optional function called with a list (`collection`,
#'   `resource`, `package`, `loader`, `root`) immediately before a loader's
#'   `open` function runs. Useful for logging and for counting reads in tests.
#'   Use `NULL` to remove it.
#' @return The current configuration, invisibly.
#' @examples
#' old <- data_config(messages = FALSE)
#' data_config(messages = old$messages)
#' @export
data_config <- function(messages, on_open) {
  cfg <- .state$config
  if (!missing(messages)) {
    stopifnot(is.logical(messages), length(messages) == 1L, !is.na(messages))
    cfg["messages"] <- list(messages)
  }
  if (!missing(on_open)) {
    stopifnot(is.null(on_open) || is.function(on_open))
    cfg["on_open"] <- list(on_open)
  }
  .state$config <- cfg
  invisible(cfg)
}
