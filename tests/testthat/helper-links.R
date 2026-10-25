# Whether this platform and file system can create directory links, probed once
# in a scratch directory that is removed again. Only tests that need a real
# staged view depend on it; mocked link-failure tests do not.
links_supported <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      dir <- tempfile("pd-linkprobe-")
      dir.create(dir)
      target <- file.path(dir, "target")
      dir.create(target)
      link <- file.path(dir, "link")
      cached <<- isTRUE(suppressWarnings(file.symlink(target, link))) && dir.exists(link)
      projectData:::remove_link(link)
      unlink(dir, recursive = TRUE)
    }
    cached
  }
})

skip_if_no_links <- function() {
  testthat::skip_if_not(links_supported(), "directory links are not supported here")
}

# Creates a link and registers its removal as a link (never recursively, so the
# target survives) when `env` exits. Registered after the enclosing fixtures, so
# it runs before their recursive tempdir cleanup.
local_link <- function(target, link, env = parent.frame()) {
  ok <- isTRUE(suppressWarnings(file.symlink(target, link)))
  if (ok) withr::defer(projectData:::remove_link(link), envir = env)
  ok
}

# Spellings of `path` a diagnostic may legitimately use: as given, canonical
# (symlinks and long names resolved), the logical form with redundant separators
# collapsed (a TMPDIR ending in "/" gives ".../T//Rtmp..." on macOS, where the
# physical form also differs: /var -> /private/var), and on Windows the 8.3
# short form. The short form is kept as returned (not re-normalized, which would
# expand it back to the long form). All use "/", have no trailing "/", and are
# lower-case, so they compare with a message treated alike.
path_spellings <- function(path) {
  forms <- c(path, normalizePath(path, winslash = "/", mustWork = FALSE))
  if (.Platform$OS.type == "windows") {
    forms <- c(forms, tryCatch(utils::shortPathName(path), error = function(e) character()))
  }
  forms <- gsub("\\", "/", forms, fixed = TRUE)
  # keep a leading "//" (Windows UNC share) while collapsing the rest
  collapse <- function(p) sub("(?<=.)/+$", "", sub("^(//?)/*", "\\1", gsub("(?<=.)/{2,}", "/", p, perl = TRUE)), perl = TRUE)
  forms <- c(forms, collapse(forms))
  tolower(unique(forms))
}

# Asserts that `msg` mentions the location `path`, whatever its spelling.
expect_mentions_path <- function(msg, path, extra = NULL) { # TEMP-DEBUG
  text <- tolower(gsub("\\", "/", msg, fixed = TRUE))
  text <- paste(text, gsub("(?<=.)/{2,}", "/", text, perl = TRUE))
  testthat::expect_true(any(vapply(path_spellings(path), grepl, NA, x = text, fixed = TRUE)),
                        info = paste(c(paste("message does not mention", path), extra), collapse = "\n"))
}

test_that("same_path treats spellings of one location as equal and others as different", {
  d <- withr::local_tempdir()
  expect_true(projectData:::same_path(d, paste0(d, "/")))
  expect_true(projectData:::same_path(d, normalizePath(d, winslash = "\\")))
  expect_false(projectData:::same_path(d, file.path(d, "other")))
})
