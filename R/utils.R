`%||%` <- function(x, y) if (is.null(x)) y else x

pd_condition <- function(message, class, base) {
  structure(
    class = c(class, "projectData_condition", base, "condition"),
    list(message = message, call = NULL)
  )
}

pd_abort <- function(..., class = character()) {
  stop(pd_condition(paste0(...), class, "error"))
}

pd_warn <- function(..., class = character()) {
  warning(pd_condition(paste0(...), class, "warning"))
}

quote_names <- function(x) paste0("'", x, "'", collapse = ", ")

is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

# Portable manifests may only reference files below the collection root.
is_safe_relative_path <- function(path) {
  if (!is_scalar_string(path)) return(FALSE)
  if (grepl("\\", path, fixed = TRUE)) return(FALSE)
  if (grepl("^(/|~|[A-Za-z]:)", path)) return(FALSE)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  !any(parts == "..") && !any(parts == "")
}

# Guards against symlinks that leave the root. `allowed` replaces the root as
# the boundary for a staged source layout (see layout.R).
resolve_inside_root <- function(root, relative, allowed = NULL) {
  full <- file.path(root, relative)
  if (!file.exists(full)) return(NA_character_)
  real <- normalizePath(full, winslash = "/", mustWork = TRUE)
  bounds <- allowed %||% normalizePath(root, winslash = "/", mustWork = TRUE)
  if (!any(startsWith(real, paste0(sub("/$", "", bounds), "/")))) return(NA_character_)
  real
}

rbind_list <- function(x, template) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (!length(x)) return(template[0, , drop = FALSE])
  out <- do.call(rbind, x)
  rownames(out) <- NULL
  out
}
