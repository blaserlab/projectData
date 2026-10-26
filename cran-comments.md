## Submission

This is a resubmission of projectData, version 0.1.0.

The initial submission's automatic CRAN check reported 2 ERRORs (0 WARNINGs,
1 NOTE) on Windows Server 2022, R-devel (GCC 14.3): a unit test and a vignette
chunk (`adapters.Rmd`) each called manifest-authoring code that stages a
temporary installed-shaped view of a package source tree by creating
directory links, and that CRAN Windows environment cannot create them.

This is expected and already handled by design: projectData deliberately
refuses to fall back to copying a data collection when a link cannot be
created (which could silently duplicate a large collection), and instead
raises an informative `projectData_staging_error`. The test and the vignette
example did not previously account for environments without directory-link
support, so they failed instead of exercising that documented error path.

Fixes in this resubmission (tests and documentation only; no production code
changed):

- The affected unit test (`test-manifest-rules.R`) now checks directory-link
  support and, where it is unavailable, asserts the `projectData_staging_error`
  is raised with no partial files or copied data, instead of assuming linking
  succeeds.
- The `adapters.Rmd` vignette's executable source-tree-authoring example was
  replaced with a portable installed-layout example that needs no directory
  links, while the vignette's prose still documents source-tree authoring and
  its link requirement, with the installed-layout alternative noted explicitly.
- Other tests exercising the same staging code already skipped correctly when
  links are unsupported, or simulated link failure directly; a shared helper
  (`local_link_failure()`) now makes that simulation available to all affected
  tests so the condition is covered deterministically on any platform.
- Removed leftover debugging output (`test-replacement.R`) unrelated to the
  CRAN failures, found while reviewing the test suite.

## Test environments

- Ubuntu 22.04, R 4.5.2 (local)
- Ubuntu Linux, R-devel (R-hub)
- Windows, R-devel (R-hub)
- macOS ARM64, R-devel (R-hub)

## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE from the local check is:

```
checking CRAN incoming feasibility ... NOTE
New submission
```

This NOTE is expected because this is the first CRAN submission of projectData.

The R-hub checks on Linux, Windows, and macOS completed successfully. Local
`R CMD check --as-cran` was re-run after the fixes above and also completed
with 0 errors, 0 warnings, and the same single NOTE.
