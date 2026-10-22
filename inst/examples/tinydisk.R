# A tiny, real disk-backed backend and its projectData adapter.
# Used by the tests and the vignette. Not part of the projectData API.
#
# Format: a directory `<name>.tinymat/` containing `dim.txt` ("nrow ncol") and
# `values.bin` (doubles, column-major). An opened `tinymat` holds only the
# directory path and dimensions; values are read from disk on `[`.
# No connection is stored, so objects survive serialization of the path only,
# and the backing files must outlive the object.

tinymat_write <- function(x, dir) {
  dir.create(dir, recursive = TRUE)
  writeLines(paste(dim(x), collapse = " "), file.path(dir, "dim.txt"))
  con <- file(file.path(dir, "values.bin"), "wb")
  on.exit(close(con))
  writeBin(as.double(x), con)
  invisible(dir)
}

tinymat_open <- function(dir) {
  d <- as.integer(strsplit(readLines(file.path(dir, "dim.txt")), " ")[[1]])
  structure(list(path = normalizePath(dir, winslash = "/"), dim = d), class = "tinymat")
}

dim.tinymat <- function(x) x$dim

`[.tinymat` <- function(x, i, j, drop = TRUE) {
  d <- x$dim
  i <- if (missing(i)) seq_len(d[1]) else i
  j <- if (missing(j)) seq_len(d[2]) else j
  con <- file(file.path(x$path, "values.bin"), "rb")
  on.exit(close(con))
  out <- matrix(NA_real_, length(i), length(j))
  for (k in seq_along(j)) {
    seek(con, where = 8 * (d[1] * (j[k] - 1)), rw = "read")
    col <- readBin(con, "double", n = d[1])
    out[, k] <- col[i]
  }
  if (drop && (length(i) == 1L || length(j) == 1L)) as.vector(out) else out
}

registerS3method("dim", "tinymat", dim.tinymat)
registerS3method("[", "tinymat", `[.tinymat`)

tinydisk_loader <- function() {
  projectData::data_loader(
    "tinydisk::tinymat",
    storage = "disk-backed",
    needs_work_dir = TRUE,
    discover = function(root, context) {
      dirs <- list.files(file.path(root, "extdata"), pattern = "\\.tinymat$", full.names = FALSE)
      lapply(sub("\\.tinymat$", "", dirs), function(nm) {
        list(resource = nm, source = paste0("extdata/", nm, ".tinymat"), objects = nm)
      })
    },
    validate = function(descriptor, root, context) {
      src <- file.path(root, descriptor$source)
      ok <- file.exists(file.path(src, "dim.txt")) && file.exists(file.path(src, "values.bin"))
      if (ok) TRUE else "tinymat directory is missing dim.txt or values.bin"
    },
    open = function(descriptor, paths, context) {
      src <- paths$source
      # Installed files are read-only; `work_copy = true` demonstrates using a
      # private writable copy that stays valid if the source disappears.
      if (identical(descriptor$options$work_copy, "true")) {
        dest <- file.path(paths$work_dir, basename(src))
        file.copy(src, paths$work_dir, recursive = TRUE)
        src <- dest
      }
      stats::setNames(list(tinymat_open(src)), descriptor$objects)
    }
  )
}
