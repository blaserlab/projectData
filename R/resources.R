# A resource record is an environment so that all aliases and sibling objects
# share one canonical opening/cache state.
new_resource <- function(collection, d) {
  rec <- new.env(parent = emptyenv())
  rec$collection <- collection
  rec$resource <- d$resource
  rec$loader <- d$loader
  rec$source <- d$source
  rec$options <- d$options
  rec$adapter <- d$adapter
  rec$descriptor <- d
  rec$state <- "unopened"
  rec$value <- NULL
  rec$observed <- NULL
  rec$error <- NULL
  rec
}

resource_state <- function(rec) {
  if (rec$state != "unopened") return(rec$state)
  if (!is.null(find_loader(rec$loader))) return("unopened")
  if (!is.null(rec$adapter) && nzchar(system.file(package = rec$adapter$package))) {
    return("unopened")
  }
  "unavailable-loader"
}

objects_table <- function(descriptors, include) {
  rows <- lapply(descriptors, function(d) {
    if (is.null(d$objects)) {
      data.frame(resource = d$resource, object = NA_character_,
                 confidence = "unknown", title = NA_character_,
                 help_topic = NA_character_, recorded_class = NA_character_,
                 stringsAsFactors = FALSE)
    } else {
      data.frame(resource = d$resource, object = d$objects,
                 confidence = if (d$origin == "manifest") "declared" else "index",
                 title = d$title, help_topic = d$help_topic,
                 recorded_class = d$recorded_class, stringsAsFactors = FALSE)
    }
  })
  tab <- rbind_list(rows, data.frame(resource = "", object = "", confidence = "",
                                     title = "", help_topic = "", recorded_class = "",
                                     stringsAsFactors = FALSE))
  apply_include(tab, include)
}

apply_include <- function(tab, include) {
  tab$exposed <- if (is.null(include)) rep(TRUE, nrow(tab)) else
    tab$object %in% include | tab$resource %in% include
  tab
}

# Opening --------------------------------------------------------------------

resolve_resource_loader <- function(rec) {
  loader <- find_loader(rec$loader)
  if (!is.null(loader)) return(loader)
  if (is.null(rec$adapter)) {
    pd_abort("No loader '", rec$loader, "' is registered, so resource '",
             rec$resource, "' of collection '", rec$collection, "' cannot be opened. ",
             "Register an adapter with data_loader_add() or data_loader_adapter(), ",
             "then retry. Other objects are unaffected.",
             class = "projectData_loader_unavailable")
  }
  loaders <- tryCatch(
    fetch_adapter_loaders(rec$adapter$package, rec$adapter$export),
    projectData_loader_unavailable = function(e) {
      pd_abort(conditionMessage(e), " (needed for loader '", rec$loader,
               "', resource '", rec$resource, "' of collection '", rec$collection,
               "'). Other objects are unaffected.",
               class = "projectData_loader_unavailable")
    }
  )
  ids <- vapply(loaders, function(l) l$id, "")
  if (!rec$loader %in% ids) {
    pd_abort("Adapter '", rec$adapter$package, "::", rec$adapter$export,
             "' does not provide loader '", rec$loader, "' (provides: ",
             paste(ids, collapse = ", "), ").", class = "projectData_loader_unavailable")
  }
  for (l in loaders[ids == rec$loader]) add_loader(l, replace = FALSE, builtin = FALSE)
  find_loader(rec$loader)
}

check_installation_unchanged <- function(co) {
  desc_file <- file.path(co$root, "Meta", "package.rds")
  now <- if (file.exists(desc_file)) installed_identity(co$root) else NA_character_
  if (!identical(now, co$identity)) {
    pd_abort("Installed package '", co$package, "' at ", co$root,
             " changed since collection '", co$id, "' was registered (registered: ",
             co$identity, "; now: ", now, "). Nothing was opened. Re-register with ",
             "data_attach(\"", co$package, "\", id = \"", co$id, "\", replace = TRUE). ",
             "Objects already opened are unaffected.",
             class = "projectData_stale_installation")
  }
}

# On-disk size of a single source file, from stat() only. Skipped for
# directories and for the shared lazy-load database, where it would mislead.
source_size_text <- function(co, rec) {
  if (is.na(rec$source) || identical(rec$source, "data/Rdata.rdb")) return("")
  path <- file.path(co$root, rec$source)
  info <- file.info(path)
  if (is.na(info$size) || isTRUE(info$isdir)) return("")
  paste0(" [", format(structure(info$size, class = "object_size"), units = "auto"),
         " on disk: ", rec$source, "]")
}

work_dir_for <- function(co, rec) {
  .state$work_counter <- .state$work_counter + 1L
  dir <- file.path(tempdir(), "projectData-work", co$id,
                   sprintf("%s-%03d", gsub("[^A-Za-z0-9._-]", "_", rec$resource), .state$work_counter))
  dir.create(dir, recursive = TRUE)
  dir
}

open_resource <- function(co, rec) {
  switch(
    rec$state,
    ready = return(invisible(rec)),
    opening = pd_abort("Resource '", rec$resource, "' of collection '", co$id,
                       "' is already being opened (reentrant access).",
                       class = "projectData_reentrant"),
    failed = pd_abort("Resource '", rec$resource, "' of collection '", co$id,
                      "' failed to open earlier: ", rec$error,
                      " Call data_retry(\"", co$id, "\", \"", rec$resource,
                      "\") to try again.", class = "projectData_opening_failed")
  )
  loader <- resolve_resource_loader(rec)
  check_installation_unchanged(co)

  cfg <- .state$config
  if (isTRUE(cfg$messages)) {
    objs <- co$objects$object[co$objects$resource == rec$resource & !is.na(co$objects$object)]
    message("projectData: opening resource '", rec$resource, "' of collection '",
            co$id, "' (", co$package, " ", co$version, ")",
            if (length(objs)) paste0(" with object(s) ", paste(objs, collapse = ", ")) else "",
            source_size_text(co, rec), ". Loader ", rec$loader,
            "; local installed files, nothing is downloaded",
            if (loader$storage == "disk-backed") "; disk-backed data are opened, not necessarily loaded into memory" else "",
            ".")
  }
  if (is.function(cfg$on_open)) {
    cfg$on_open(list(collection = co$id, resource = rec$resource,
                     package = co$package, loader = rec$loader, root = co$root))
  }

  rec$state <- "opening"
  ok <- FALSE
  on.exit(if (!ok && rec$state == "opening") {
    rec$state <- "failed"
    rec$error <- rec$error %||% "opening was interrupted."
  })
  tryCatch({
    d <- rec$descriptor
    paths <- list(
      root = co$root,
      source = if (is.na(rec$source)) NA_character_ else resolve_inside_root(co$root, rec$source),
      work_dir = if (isTRUE(loader$needs_work_dir)) work_dir_for(co, rec) else NULL
    )
    if (!is.na(rec$source) && is.na(paths$source)) {
      pd_abort("Source file '", rec$source, "' no longer exists below ", co$root, ".")
    }
    context <- list(collection = co$id, package = co$package, version = co$version,
                    library = co$library, resource = rec$resource)
    value <- loader$open(d, paths, context)
    if (!is.list(value) || (length(value) && (is.null(names(value)) || anyNA(names(value)) ||
                                              any(!nzchar(names(value))) || anyDuplicated(names(value))))) {
      pd_abort("Loader '", rec$loader, "' must return a uniquely named list of objects.")
    }
    rec$value <- value
    rec$observed <- names(value)
    rec$error <- NULL
    rec$state <- "ready"
    ok <- TRUE
  }, error = function(e) {
    rec$state <- "failed"
    rec$error <- conditionMessage(e)
    rec$value <- NULL
    stop(pd_condition(
      paste0("Opening resource '", rec$resource, "' of collection '", co$id,
             "' failed: ", conditionMessage(e), " The resource is marked 'failed'; ",
             "call data_retry(\"", co$id, "\", \"", rec$resource, "\") to try again."),
      "projectData_opening_failed", "error"))
  })
  invisible(rec)
}

get_object <- function(id, object) {
  co <- get_collection(id)
  row <- co$objects[!is.na(co$objects$object) & co$objects$object == object, , drop = FALSE]
  if (!nrow(row)) pd_abort("Collection '", id, "' has no object '", object, "'.")
  rec <- co$resources[[row$resource[1]]]
  open_resource(co, rec)
  if (!object %in% names(rec$value)) {
    pd_abort("Resource '", rec$resource, "' of collection '", id, "' opened but did not ",
             "produce '", object, "'. It produced: ",
             if (length(rec$value)) quote_names(names(rec$value)) else "nothing",
             ". The recorded mapping is wrong; see data_inspect().",
             class = "projectData_mapping_error")
  }
  rec$value[[object]]
}
