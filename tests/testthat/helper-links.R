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
      unlink(link, recursive = FALSE)
      unlink(dir, recursive = TRUE)
    }
    cached
  }
})

skip_if_no_links <- function() {
  testthat::skip_if_not(links_supported(), "directory links are not supported here")
}
