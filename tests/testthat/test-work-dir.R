test_that("path_component is injective and yields one safe component", {
  pc <- projectData:::path_component
  ids <- c("pkg", "a/b", "a_b", "a\\b", "a%2Fb", "a.b", ".", "..", "../x", "/abs",
           "A", "a", "", "%", "%25", "caf\u00e9", "a b", "x-001")
  out <- vapply(ids, pc, character(1))
  expect_false(anyDuplicated(out) > 0)
  # Case-insensitive filesystems must not see collisions either.
  expect_false(anyDuplicated(tolower(out)) > 0)
  expect_true(all(grepl("^[a-z0-9%A-F-]+$", out)))
  expect_false(any(out %in% c(".", "..")))
  expect_identical(pc("pkg"), "pkg")
  expect_error(pc(NA_character_))
  expect_error(pc(c("a", "b")))
})

test_that("work_dir_for stays under projectData-work and is distinct per identity", {
  root <- normalizePath(file.path(tempdir(), "projectData-work"), winslash = "/", mustWork = FALSE)
  withr::defer(unlink(file.path(tempdir(), "projectData-work"), recursive = TRUE))
  ids <- c("pkg", "a/b", "a_b", "../escape", "..", "/abs/path", "a\\b", "")
  res <- c("res", "r/s", "r_s", "../../up", ".", "x y")
  grid <- expand.grid(id = ids, resource = res, stringsAsFactors = FALSE)
  dirs <- Map(function(i, r) {
    projectData:::work_dir_for(list(id = i), list(resource = r))
  }, grid$id, grid$resource) |> unlist(use.names = FALSE)

  expect_true(all(dir.exists(dirs)))
  expect_false(anyDuplicated(dirs) > 0)
  norm <- normalizePath(dirs, winslash = "/")
  expect_true(all(startsWith(norm, paste0(root, "/"))))
  # Exactly <root>/<collection>/<resource>-NNN: nothing nested deeper.
  rel <- substring(norm, nchar(root) + 2L)
  expect_true(all(lengths(strsplit(rel, "/", fixed = TRUE)) == 2L))
  # Nothing was written outside the root.
  expect_false(dir.exists(file.path(tempdir(), "escape")))
  expect_false(dir.exists(file.path(dirname(tempdir()), "escape")))
})

test_that("work_dir_for never reuses an existing directory", {
  withr::defer(unlink(file.path(tempdir(), "projectData-work"), recursive = TRUE))
  co <- list(id = "same"); rec <- list(resource = "r")
  d1 <- projectData:::work_dir_for(co, rec)
  file.create(file.path(d1, "live"))
  d2 <- projectData:::work_dir_for(co, rec)
  expect_false(identical(d1, d2))
  expect_true(file.exists(file.path(d1, "live")))
})
