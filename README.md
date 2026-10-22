# projectData

projectData registers installed data packages ("collections") for a session
without reading their data, exposes objects through deferred bindings, resolves
name conflicts through explicit choices (a primary collection, per-object
preferences, optional suffix aliases), records provenance, and provides a
loader interface for resources that need format-specific restoration.

Status: development version, not submitted to CRAN. Author, maintainer and
license details in `DESCRIPTION` are placeholders pending confirmation.

## Quick start

```r
library(projectData)
data_register(c("mainData", "refData"), id = c("main", "reference"),
              suffix = c(NA, ".reference"), primary = "main")
data_catalog()    # metadata only; nothing is opened
data_attach()     # optional facade for bare names
cds_main          # opened on first use
data_get("cds_main", collection = "reference")
```

See `vignette("projectData")` and `vignette("adapters")`. Installation of data
packages (including from tarballs) is separate from registration.

## Scope and limits

* Not a package manager, not an out-of-memory computation engine, and no
  guarantee of reproducibility: class compatibility, software versions,
  backing files and platform readers still matter.
* `conflicted` and `library(include.only = )` handle export conflicts and
  selection; projectData adds collection inventory, deferred access beyond lazy
  data, format restoration and provenance.
* ExperimentHub/remote retrieval is a possible future extension, not
  implemented.
* IDE completion or previews may force deferred bindings; see
  `inst/ide-repro/`.
