# projectData 0.1.0

* Initial CRAN release.
* Register installed R data packages as named collections.
* Provide deferred access and metadata-only resource inventories.
* Support explicit name-conflict resolution and source-qualified access.
* Preserve package provenance and installation identity.
* Introduce a public loader interface for specialized data formats.

# projectData 0.0.0.9007 (development version)

## Changes in this version

* Changed installations are now detected reliably. The registered installation
  is compared with the metadata on disk at its recorded root (never a loaded
  namespace), and the library entry must still lead to that root. Previously a
  library link repointed to another installation (pak/renv-style stores) left
  the physically resolved root untouched, so `data_attach()` was a silent no-op
  and bindings stayed stale. The identity also covers every file in `Meta/`
  and the manifest, so same-version reinstalls are caught without relying on one
  timestamp.
* The stale-installation error names the collection, path, registered and
  current version and the exact `data_attach(..., replace = TRUE)` command, and
  states that objects already held elsewhere keep the old value. It is raised
  before registration, bindings, preferences or attachment change.
* `data_attach(replace = TRUE)` for an existing id keeps omitted `lib.loc`
  (the library of the current installation, not `.libPaths()`), `suffix` and
  `include`; explicit arguments win. A failed replacement keeps the previous
  state (class `projectData_replace_failed`). Old resource records and caches are
  discarded whenever the installation changed. A `lib.loc` that selects a
  different installation for an existing id now fails with
  `projectData_installation_conflict` (and with the stale-installation error if
  the registered one changed) unless `replace = TRUE`.
* Resource working directories (for loaders that need a writable copy) are now
  built from safe path components. Both the collection id and the resource name
  are encoded injectively (only `a-z`, `0-9` and `-` pass through; every other
  byte, including uppercase letters, becomes `%XX`), so path separators or `..`
  can no longer move a directory out of `projectData-work`, and distinct
  identities such as `a/b` and `a_b` never share a directory. Directory names for
  ids or resources containing uppercase letters, `_` or `.` differ from earlier
  versions. Caching, directory reuse and lifetime are unchanged; existing
  directories are never deleted.

# projectData 0.0.0.9006

## Changes in this version

* Manifest validation and authoring against a verified package source tree now
  accept `Source` paths that pass through a symbolic link under `inst/` (e.g.
  `inst/extdata -> /shared/data`), provided the final path exists. `Source`
  stays package-relative. `data/`, installed packages and standalone
  directories keep strict physical containment, and a directory with
  installation artifacts (`Meta/`, `R/<pkg>.rdb`) is never treated as a source
  tree. Authors must still verify that built archives contain the data.

# projectData 0.0.0.9005

## Changes in this version

* One rule now selects the file behind a dataset. For `data/` files, the
  built-in reader and the inventory both use the selection `utils::data()`
  makes: a name in the lazy-load map wins; otherwise the first supported
  extension in `data()`'s precedence order (`R`, `r`, `RData`, `rdata`, `rda`,
  then tables, then csv) for which `<dataset>.<extension>` exists. Previously
  inventory recorded whichever matching file was listed last, so with both
  `dual.R` and `dual.rda` the catalog could name the `.rda` while the script was
  opened, and removing the unused `.rda` could block retrieval. Now catalog
  `source` and the first-access message name the file actually read, lazy-load
  datasets still share `data/Rdata.rdb`, and removing an unused alternative has
  no effect. If the selected file is removed, opening fails with an error naming
  it and never falls back to another file; if a file with higher precedence has
  appeared since registration, opening is refused with a request to re-register
  (`replace = TRUE`) instead of silently reading something other than the
  recorded source (a dataset registered with no located file records no source,
  so a file restored later is simply read). Selection uses the directory listing
  and the small lazy-load map only. Files whose extension `data()` does not understand (for example
  `x.rda.gz`) are now reported as unsupported instead of being inventoried.
* Cleanup of the temporary view used to author against a package source tree is
  portable and checked. Links are removed as links and the verified-empty
  staging directory with `unlink()` instead of `file.remove()`; only entries
  created by the current operation are removed, and a link that was replaced by
  something else, or a directory that gained foreign files, is left in place.
  Link targets are never followed or deleted. Every removal is checked, and a
  failure is reported with a `projectData_cleanup_warning` naming what remains,
  without replacing the authoring error that triggered the cleanup. Cleanup is
  safe after partial staging, after success, and when repeated. Nothing is ever
  copied. This was developed and tested on Linux; it has not been run on
  Windows.
* The search-path environment that `data_attach()` creates is now removed by
  identity (its current position) in `data_detach()` and when the package is
  unloaded, and is captured by identity when it is attached. An unrelated
  search-path entry with the same name is left alone.
* Adapter work directories skip names that already exist, so reloading the
  package inside one R session no longer collides with directories made before.
* Package metadata: authors, maintainer and copyright holder are filled in from
  information in the repository (see `DESCRIPTION`); README, `DESCRIPTION` and
  `LICENSE` agree on the MIT license. The optional monocle3/BPCells check moved
  to `dev/integration/`, outside the package tests and the source tarball.
  Minimum versions of the test packages are declared (testthat 3.2.0 for
  `local_mocked_bindings()`, withr, tools), and tests remove the libraries and
  scratch files they create.

## Behavior of this version

### Attaching and retrieving

* `data_attach()` registers installed data packages (metadata only) and
  attaches them in one step as deferred active bindings in an environment named
  `projectData:data`. Nothing is attached when projectData loads. Objects are
  read on first use, after a message (`data_config(messages = )`).
* `data_get(name, package = )` registers the package automatically if needed
  and returns the object without attaching anything; explicit and attached
  access share one resource cache. `data_query()` looks at packages without
  remembering them, `data_detach()` removes attached collections and forgets
  every registration, attached or not. It never deletes files.
* Name conflicts are never resolved by attachment order. Use a primary
  collection (`data_primary()`), per-object preferences (`data_prefer()`),
  suffix aliases or qualified `data_get(name, collection = )`. Resolution uses
  names only.
* Installed packages are resolved once, with their installation identity; if the
  installation changes before an object is opened, opening fails and asks for
  `replace = TRUE`.

### Opening, caching, failure

* A resource is the loadable unit and can produce several objects; aliases and
  siblings share one canonical opening. Opening is guarded against reentrancy;
  a failed resource stays failed until `data_retry()`. `on_open` runs inside
  that guard.
* Object names recorded at registration are unchecked until a resource is
  opened. `data_get()` is strict about a missing object; `data_inspect()`
  replaces the recorded mapping with the observed names (rejecting duplicates
  within a collection, atomically).
* Script datasets (`data/*.R`) run only on first access, in a fresh output
  environment whose enclosing lookup chain is the fixed set of standard startup
  packages (stats, graphics, grDevices, utils, datasets, methods, base) and
  excludes the global workspace and attached packages. The chain is built only
  for script datasets. `utils` is detached again after `utils::data()` when it
  was not attached beforehand. See `?data_attach`.

### Catalog and provenance

* `data_catalog()` is metadata only: it never opens a resource or inspects
  values. Columns include `origin` (`index`, `adapter`, `manifest`), `mapping`
  (`recorded`, `unknown`, `verified`, `mismatch`), `loader_status`, `storage`
  and `state`. `storage` of a ready resource is the snapshot taken when it was
  opened, so later loader changes do not relabel it.

### Loaders and adapters

* `data_loader()` defines contract version 1 (`discover`, `validate`, `open`);
  `data_loader_add()` and `data_loader_adapter()` register loaders atomically,
  and nothing is installed automatically. `validate` receives the same context
  as `open` and runs before every uncached opening. `projectData::rds` and
  `projectData::dataset` are built in.
* Adapter claims on standard datasets are ignored with a warning
  (`data_config(ignored_claims = )`); a manifest entry assigns such a resource to
  a specialized reader. Ambiguous adapter claims are errors.
* Disk-backed objects: detaching never deletes backing files; an object that
  references installed files can still break if the installation is replaced or
  removed. Adapters state whether they return installed-file references or a
  working copy (`needs_work_dir`).

### Manifests

* An optional versioned DCF file, `inst/projectData/manifest.dcf`, declares
  loaders and mappings as data (never code). Reading, `data_manifest_validate()`
  and `data_manifest_write()` apply the same rules: one declaration per resource,
  unique object names across resources, strictly parsed `Options`.
* `data_manifest_draft()` and `data_manifest_write()` author manifests from the
  registered adapters' discovery. `root` can be a package source tree (needs
  symbolic links, see above), an installed package, or a standalone directory;
  `Source` paths are always installed-relative. Existing manifests are never
  overwritten without `overwrite = TRUE`, and nothing is written inside an
  installed package.

### Not included

* A monocle3/BPCells adapter is deferred; the tiny disk-backed backend in
  `inst/examples/tinydisk.R` is the tested adapter example.
* ExperimentHub or other remote retrieval is a possible extension, not
  implemented.
