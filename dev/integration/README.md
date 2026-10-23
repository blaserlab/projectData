# Optional integration checks (development only)

These checks need packages that projectData deliberately does not depend on
(monocle3, BPCells), so they are kept out of `tests/` and of the source
tarball. Each test skips itself when its packages are missing. They exercise a
test-local loader, not a shipped adapter, and they are not part of the CRAN
checks. Monocle adapter development is deferred.

Run from the package root:

```r
testthat::test_dir("dev/integration", package = "projectData", load_package = "source")
```
