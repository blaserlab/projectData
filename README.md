# projectData

projectData attaches installed data packages ("collections") in a session
without reading their data, exposes objects through deferred bindings that are
read on first use (with a message beforehand), resolves name conflicts through
explicit choices (a primary collection, per-object preferences, optional suffix
aliases), records provenance, and provides a loader interface for resources that
need format-specific restoration.

Status: development version (see `NEWS.md`), not submitted to CRAN.

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

## Where it fits

`data()`, lazy loading and `pkg::object` already work well for a single data
package, and projectData complements them. It addresses coordination problems
that arise when a project uses several independently distributed collections:
overlapping object names, large or disk-backed resources that should stay
unopened until used, formats that need their own restoration step, and the need
to see which installed package version and library each object came from.
Ordinary data packages work without changes; optional manifests and adapters
cover special formats. It does not install or update data packages and does not
replace CRAN, Bioconductor or Zenodo. If `data()` already meets your needs, you
can keep using it.

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
* Authoring manifests against a package source tree needs a file system that
  can create symbolic links; installed packages and prepared directories do not.
* Monocle3/BPCells (CDS) support is deferred. It would be a separate adapter
  package; the core has no dependency on either. The vignette `adapters` shows
  a working adapter with the small disk-backed example backend.

## License

MIT, copyright holder Brad Blaser. `DESCRIPTION` declares `MIT + file LICENSE`;
`LICENSE` holds the year and copyright holder, and `LICENSE.md` the full text.
