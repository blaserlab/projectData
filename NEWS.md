# projectData (development version)

* `data_config(ignored_claims = )` sets how ignored adapter claims on standard datasets are reported: `"warn"` (default), `"message"` or `"silent"`. Ambiguous adapter collisions remain errors.
* Discovery collisions are now resolved before they are rejected. A manifest entry replaces all competing candidates for its resource (so it can resolve adapter-versus-adapter ambiguity or assign a standard dataset to a specialized reader). Without a manifest entry, resources in the standard data inventory stay with the built-in reader and adapter claims on them are ignored with a `projectData_ignored_claim` warning, rather than failing. Adapter-versus-adapter collisions on other resources still fail until a manifest decides. Duplicate manifest records for one resource are rejected with the loaders named.
* `data_inspect()` can now correct wrong object mappings. Opening a resource no longer fails when the loader's outputs do not match the recorded names; only a loader error or an empty result is an opening failure. `data_get()` stays strict: an absent object gives an error naming the resource, the expected object and the observed names, and points to `data_inspect()`. Inspection replaces the mapping with the observed names (complete or partial mismatches), and the catalog `mapping` column gains `mismatch` for recorded names a ready resource did not produce. Opened resources stay cached and shared.

# projectData 0.0.0.9001

* Script-based package datasets (`data/*.R`) now load. The built-in dataset loader evaluates them in an isolated environment whose parent is `baseenv()` (previously `emptyenv()`, which could not resolve `<-` or any function). Scripts can use base functions only; they cannot see the global workspace or attached packages, and must qualify other functions with `pkg::` (see the "Script datasets" section of `?data_attach`). Inventory and attachment still never run scripts.

# projectData 0.0.0.9000

* First working core. `data_attach()` registers and attaches packages in one step; `data_query()` looks at packages without changing session state; `data_detach()` removes and forgets collections; `data_get()` accepts several names.
* inventory of installed data packages without reading data
  (lazy and non-lazy), attached collections, primary/preference/suffix-alias conflict
  handling, optional facade, catalog/info/help, resource-level caching,
  failure/retry, manifests, loader adapter interface, tiny disk-backed example
  backend, vignettes and manual IDE reproduction.
