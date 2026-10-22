# projectData 0.0.0.9001

* Script-based package datasets (`data/*.R`) now load. The built-in dataset loader evaluates them in an isolated environment whose parent is `baseenv()` (previously `emptyenv()`, which could not resolve `<-` or any function). Scripts can use base functions only; they cannot see the global workspace or attached packages, and must qualify other functions with `pkg::` (see the "Script datasets" section of `?data_attach`). Inventory and attachment still never run scripts.

# projectData 0.0.0.9000

* First working core. `data_attach()` registers and attaches packages in one step; `data_query()` looks at packages without changing session state; `data_detach()` removes and forgets collections; `data_get()` accepts several names.
* inventory of installed data packages without reading data
  (lazy and non-lazy), attached collections, primary/preference/suffix-alias conflict
  handling, optional facade, catalog/info/help, resource-level caching,
  failure/retry, manifests, loader adapter interface, tiny disk-backed example
  backend, vignettes and manual IDE reproduction.
