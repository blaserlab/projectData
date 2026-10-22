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
data_attach(c("rnaseqData", "proteinData"))  # inventory + attach; no data read
counts_rna                                   # opened on first use
data_catalog()                               # metadata only
```

If two attached packages define the same name, resolve it with
`data_prefer()`, `data_primary()` or a `suffix`, or use
`data_get(name, collection = )`. `data_register()` builds the inventory without
attaching, for inspecting a package first.

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
