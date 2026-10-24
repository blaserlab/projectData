# Symlinked inst/ resources in a verified source tree (and nowhere else).

write_ext_manifest <- function(file, source, id = "extres") {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("ManifestVersion: 1", "", paste("Resource:", id), "Loader: extadp::thing",
               paste("Source:", source), paste("Objects:", id)), file)
  file
}

# A source package whose inst/extdata is a link to a separate directory that
# holds the resource directory `res`.
local_symlinked_pkg <- function(env = parent.frame()) {
  skip_if_no_links()
  fx <- local_source_pkg(env = env)
  ext <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(ext, "res"))
  writeLines("x", file.path(ext, "res", "cds_object.rds"))
  unlink(file.path(fx$src, "inst", "extdata"), recursive = TRUE)
  file.symlink(ext, file.path(fx$src, "inst", "extdata"))
  c(fx, list(ext = ext, manifest = file.path(fx$src, "inst", "projectData", "manifest.dcf")))
}

test_that("A: a source package may link inst/extdata outside the tree", {
  fx <- local_symlinked_pkg()
  write_ext_manifest(fx$manifest, "extdata/res")
  back <- data_manifest_validate(fx$manifest)
  expect_equal(back[[1]]$source, "extdata/res")
  expect_false(startsWith(normalizePath(file.path(fx$src, "inst", "extdata", "res")), fx$src))
  expect_length(stage_dirs(), 0L)
})

test_that("B: broken symlinks and missing resources fail", {
  fx <- local_symlinked_pkg()
  write_ext_manifest(fx$manifest, "extdata/missing")
  expect_error(data_manifest_validate(fx$manifest), "does not exist")
  write_ext_manifest(fx$manifest, "extdata/res")
  unlink(fx$ext, recursive = TRUE)
  expect_error(data_manifest_validate(fx$manifest), "does not exist|Could not")
})

test_that("C and D: traversal and absolute paths are rejected even when the target exists", {
  fx <- local_symlinked_pkg()
  write_ext_manifest(fx$manifest, "extdata/../extdata/res")
  expect_error(data_manifest_validate(fx$manifest), "relative path")
  write_ext_manifest(fx$manifest, file.path(fx$ext, "res"))
  expect_error(data_manifest_validate(fx$manifest), "relative path")
})

test_that("data/ symlinks leaving the tree stay rejected in a source tree", {
  skip_if_no_links()
  fx <- local_source_pkg()
  out <- withr::local_tempdir()
  writeLines("x", file.path(out, "outside.rda"))
  file.symlink(file.path(out, "outside.rda"), file.path(fx$src, "data", "outside.rda"))
  mf <- write_ext_manifest(file.path(fx$src, "inst", "projectData", "manifest.dcf"), "data/outside.rda")
  expect_error(data_manifest_validate(mf), "does not exist inside the package root")
})

test_that("E: an installed package never follows a link out of its root", {
  skip_if_no_links()
  local_clean_session()
  fx <- local_source_pkg()
  lib <- fixture_library()
  inst <- install_source_pkg(fx$src, lib)
  ext <- withr::local_tempdir()
  dir.create(file.path(ext, "res"))
  unlink(file.path(inst, "extdata"), recursive = TRUE)
  file.symlink(ext, file.path(inst, "extdata"))
  mf <- write_ext_manifest(file.path(inst, "projectData", "manifest.dcf"), "extdata/res")
  expect_error(data_manifest_validate(mf), "does not exist inside the package root")
  # Naming the source layout for an installed directory cannot relax the rule.
  expect_error(data_manifest_validate(mf, root = inst, layout = "source"), "installed package")
})

test_that("an installed-looking directory is never treated as a source tree", {
  skip_if_no_links()
  fx <- local_symlinked_pkg()
  dir.create(file.path(fx$src, "Meta"))
  expect_equal(detect_layout(fx$src), "installed")
  write_ext_manifest(fx$manifest, "extdata/res")
  expect_error(data_manifest_validate(fx$manifest, root = fx$src, layout = "source"), "installed package")
})

test_that("F: ordinary resources validate in source and installed contexts", {
  skip_if_no_links()
  local_clean_session()
  fx <- local_source_pkg()
  mf <- write_ext_manifest(file.path(fx$src, "inst", "projectData", "manifest.dcf"),
                           "extdata/counts.tinymat")
  expect_equal(data_manifest_validate(mf)[[1]]$source, "extdata/counts.tinymat")
  inst <- install_source_pkg(fx$src, fixture_library())
  expect_equal(data_manifest_validate(file.path(inst, "projectData", "manifest.dcf"))[[1]]$source,
               "extdata/counts.tinymat")
})

test_that("G: authoring keeps the logical Source for a symlinked resource", {
  fx <- local_symlinked_pkg()
  local_clean_session()
  loader <- data_loader("extadp::thing",
    discover = function(root, context) {
      list(list(resource = "res", source = "extdata/res", objects = "res"))
    },
    validate = function(descriptor, root, context) file.exists(file.path(root, descriptor$source)),
    open = function(descriptor, paths, context) list(res = 1))
  data_loader_add(loader)
  d <- data_manifest_draft(fx$src)
  data_manifest_write(d, fx$manifest)
  txt <- readLines(fx$manifest)
  expect_true("Source: extdata/res" %in% txt)
  expect_false(any(grepl(fx$ext, txt, fixed = TRUE)))
  expect_equal(data_manifest_validate(fx$manifest)[[1]]$source, "extdata/res")
})

test_that("H: a built and installed symlinked package is self-contained", {
  skip_on_cran()
  fx <- local_symlinked_pkg()
  write_ext_manifest(fx$manifest, "extdata/res")
  builddir <- withr::local_tempdir()
  out <- system2(file.path(R.home("bin"), "R"),
                 c("CMD", "build", "--no-build-vignettes", shQuote(fx$src)),
                 stdout = TRUE, stderr = TRUE, env = paste0("TMPDIR=", tempdir()))
  tarball <- list.files(".", "^pdsrc_.*\\.tar\\.gz$", full.names = TRUE)
  withr::defer(unlink(tarball))
  skip_if(!length(tarball), paste("R CMD build unavailable:", paste(utils::tail(out, 3), collapse = " ")))
  untar(tarball, exdir = builddir)
  built <- file.path(builddir, "pdsrc", "inst", "extdata", "res", "cds_object.rds")
  expect_true(file.exists(built))
  expect_true(is.na(link_target(file.path(builddir, "pdsrc", "inst", "extdata"))))

  lib <- fixture_library()
  inst <- install_source_pkg(file.path(builddir, "pdsrc"), lib)
  expect_equal(data_manifest_validate(file.path(inst, "projectData", "manifest.dcf"))[[1]]$source,
               "extdata/res")
})
