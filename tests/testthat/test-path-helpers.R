# Portability helpers: path spellings and tree snapshots.

test_that("path_spellings and expect_mentions_path accept equivalent spellings only", {
  d <- withr::local_tempdir()
  expect_mentions_path(paste("at", d, "was replaced"), d)
  expect_mentions_path(paste("at", gsub("/", "\\", d, fixed = TRUE), "was replaced"), d)
  expect_mentions_path(paste("at", toupper(d), "x"), d)
  expect_mentions_path(paste("at", normalizePath(d, winslash = "/"), "x"), d)
  expect_true(all(grepl("/", path_spellings(d), fixed = TRUE)))
  expect_false(any(grepl("\\", path_spellings(d), fixed = TRUE)))
  expect_failure(expect_mentions_path("at /somewhere/else", d))
})

test_that("tree_files reports sorted relative paths with forward slashes", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "sub", "deeper"), recursive = TRUE)
  writeLines("x", file.path(d, "sub", "deeper", "f.txt"))
  writeLines("y", file.path(d, "a.txt"))
  tf <- tree_files(d)
  expect_identical(tf$f, c("a.txt", "sub", "sub/deeper", "sub/deeper/f.txt"))
  expect_false(anyNA(tf$sum[!tf$f %in% c("sub", "sub/deeper")]))
})

test_that("path_spellings covers redundant separators and trailing slashes", {
  d <- withr::local_tempdir()
  logical <- gsub("/+", "/", d)
  messy <- sub("/([^/]+)$", "//\\1", d)
  # a diagnostic with collapsed separators matches a path built with "//"
  expect_mentions_path(paste("at", logical, "x"), messy)
  expect_mentions_path(paste("at", logical, "x"), paste0(messy, "/"))
  expect_mentions_path(paste("at", messy, "x"), logical)
  expect_mentions_path(paste("at", gsub("/", "\\", logical, fixed = TRUE), "x"), messy)
  expect_failure(expect_mentions_path("at /elsewhere/pdr2", messy))
})
