# projectData development instructions

## Purpose

Build `projectData` as a lightweight R package intended for CRAN. It makes installed, documented data collections available through deferred access, explicit name resolution, provenance, and optional storage-format loaders. The typical workflow is one primary data package plus supplementary collections, including large disk-backed data.

The detailed starting specification is `projectData-development-prompt.md`. Existing `project_data.R` in blaserlab/blaseRtools is a behavioral reference:
https://github.com/blaserlab/blaseRtools/blob/master/R/project_data.R

Read the source when available. Verify license/attribution before reusing code. Preserve useful behavior, not lab-specific cache dependencies or incidental implementation details.

## Non-negotiable design principles

- Ordinary installed data packages work without author changes or a required manifest.
- Inventory/register collections without loading dataset values. Unknown object mappings remain explicitly unknown until the user requests inspection.
- Distinguish loadable resources/datasets from the objects they produce. Do not assume filenames, dataset names, and object names are identical.
- Resolve package installation roots explicitly. Track package version and source identity; never choose another installed copy accidentally at first access.
- Use internal collection/resource records and an optional attached facade. Offer unattached/qualified access. Never attach data automatically when projectData itself is loaded.
- No silent winner based on attachment order. Support an explicitly designated primary collection, explicit preferences, optional suffix aliases, and informative unresolved-conflict errors.
- Validate final names before committing registration changes. Do not overwrite global objects or modify unrelated environments.
- Catalog operations and conflict/preference resolution use metadata only. Do not retrieve values to check identity or type.
- Share one canonical load/cache operation across aliases and known outputs from the same resource. Protect against reentrant opening and define explicit retry after failure.
- Core readers support ordinary datasets and explicitly described RDS objects. Specialized discovery/opening lives behind a public, versioned adapter interface.
- No core dependency on blaseRtools, blaseRtemplates, monocle3, or BPCells. Adapter packages can be distributed independently; never auto-install them.
- Manifests are optional, versioned, validated additions for special formats/mappings. They contain relative paths and declarative loader references, not executable code strings or serialized functions.
- Do not enumerate extdata and assume all files are public data objects.
- Namespace-load optional dependencies on access; do not attach them. Missing specialized loaders must not block unrelated objects.
- Treat installed data as read-only. Allocate distinct work locations when an adapter requires writable files. Preserve backing-file lifetime for returned objects; detach is not permission to delete live resources.
- Report ready/opened state accurately: opening a disk-backed object does not mean its matrix was loaded into RAM.
- Loading, installation, distribution, and computation are separate concerns. Do not recreate the lab package manager. ExperimentHub/Zenodo retrieval is an extension point, not a first-release requirement.

## Scope and implementation style

Implement a complete, small core before adding optional integrations. Use base R where clear; add dependencies only for a demonstrated need. Prefer small composable functions, explicit state, and ordinary data frames for catalogs. Select S3/S4 only when justified, not for architectural appearance.

Use roxygen2 for exported documentation and NAMESPACE generation. Qualify external calls and keep exports deliberate. Explain overlap with conflicted and base selective attachment accurately: they can work with exported data, but do not provide this collection inventory/restoration framework.

Keep provenance/help mapping connected to original package topics even for aliases. Catalog print/show methods must not force data, invoke help, inspect object dimensions/classes, or estimate object size. Recorded metadata is optional and should be labeled.

Use an isolated environment when loading .rda resources. Never select an arbitrary `ls()` result as the desired value. Account for datasets that create multiple outputs and data scripts that need a compatible evaluation environment. Do not silently run scripts during inventory.

Adapter discovery/validation must be nonforcing. A loader receives resolved paths and explicit context/options and returns the usable resource/object. Use native restoration APIs for disk-backed formats. Resolve discovery precedence deterministically; ambiguous discovery must fail clearly.

Do not claim that active bindings, promises, or qualified environments eliminate IDE-induced loading. Provide a manual RStudio reproduction and document explicit-access alternatives. Avoid call-stack heuristics that return different data to previews and user code.

## Workflow

1. Inspect existing files and tests before editing. Respect unrelated user changes.
2. Briefly explain consequential architecture choices, then implement. Resolve routine details independently; ask only when a material decision cannot be inferred.
3. Keep API decisions and unsupported cases documented. Clearly distinguish implemented behavior, planned extensions, and optional integrations not exercised locally.
4. Do not publish, submit to CRAN, push commits, change remote repositories, or install into a shared user library without an explicit task authorizing it. Temporary test libraries are appropriate.
5. Never read private lab datasets merely to develop the core. Use synthetic small fixtures.

## Meaningful validation

Use testthat and temporary installed fixture packages/libraries. Test both lazy package data and non-lazy datasets; duplicate names across collections; multi-object datasets; ambiguous mappings; aliases/preferences; missing readers; failed opening and retry; registration rollback; unregister/detach behavior; relocation of backing files; and explicit package-library selection.

Instrument reader counts to prove metadata operations do not open data and multiple aliases share a successful resource read. Verify `.GlobalEnv` is untouched. Restore `.libPaths()`, search-path changes, options, loader registries, and test-owned directories. Tests must be offline, deterministic, and independent of monocle3/BPCells/ExperimentHub.

Demonstrate an actual tiny disk-backed test backend, not just a mock returning an in-memory matrix. Mock adapters are also useful for errors and counting reads. Keep real CDS integration checks separate and optional. Never equate a small-fixture pass with validation of multi-gigabyte datasets.

After code changes, run relevant tests, regenerate documentation if needed, build the package, and run `R CMD check --as-cran` when available. Investigate warnings/notes and report unresolved items honestly. Do not repeat expensive checks unless changes or failures justify it.

## CRAN preparation

- No internet access, package installation, large downloads, or user-data opening in examples/tests/vignettes or startup hooks.
- No writes into installed packages, the user's working directory, or arbitrary home directories. Use temporary directories for ephemeral work and standard user cache locations only for explicit persistent caching.
- Keep fixtures small and development materials out of the built package using appropriate build exclusions. Retain AGENTS.md in the development repository; exclude it from the CRAN source tarball.
- Verify current CRAN policy and projectData name availability before submission preparation. Check portability on available platforms and document gaps.
- Do not invent an author, maintainer, email, or copyright/license facts. Confirm these before declaring the submission artifacts complete.

## Completion report

State the behavior delivered, files changed, public API, tests/check results, limitations, and any unverified optional backends. Distinguish remaining functionality from environmental blockers. Never claim an installation, integration test, CRAN readiness, or submission succeeded unless it actually did.
