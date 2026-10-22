# projectData

projectData attaches installed data packages ("collections") in a session
without reading their data, exposes objects through deferred bindings that are
read on first use (with a message beforehand), resolves name conflicts through
explicit choices (a primary collection, per-object preferences, optional suffix
aliases), records provenance, and provides a loader interface for resources that
need format-specific restoration.

Status: development version, not submitted to CRAN. Author, maintainer and
license details in `DESCRIPTION` are placeholders pending confirmation.

## Quick start

```r
library(projectData)
data_attach(c("rnaseqData", "proteinData"))  # inventory + attach; no data read
counts_rna                                   # message, then opened on first use
data_catalog()                               # metadata only
data_get(c("counts_rna", "meta_rna"))        # explicit access, several at once
```

If two attached packages define the same name, resolve it with
`data_prefer()`, `data_primary()` or a `suffix`, or use
`data_get(name, collection = )`. `data_query()` shows what packages contain and
which names would collide, without attaching or remembering anything.
`data_detach()` removes collections and forgets them.

See `vignette("projectData")` and `vignette("adapters")`. Installation of data
packages (including from tarballs) is separate from attaching them.

## Scope and limits

* Not a package manager, not an out-of-memory computation engine, and no
  guarantee of reproducibility: class compatibility, software versions,
  backing files and platform readers still matter.
* Detaching never deletes backing files, so objects you hold can keep reading
  them. Replacing or removing the installed package is a different event:
  disk-backed objects that reference installed files may stop working.
* `conflicted` and `library(include.only = )` handle export conflicts and
  selection; projectData adds collection inventory, deferred access beyond lazy
  data, format restoration and provenance.
* ExperimentHub/remote retrieval is a possible future extension, not
  implemented.
* IDE completion or previews may force deferred bindings; see
  `inst/ide-repro/`.
