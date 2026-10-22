reserved_names <- c("...", ".Random.seed", ".Last.value", ".packageName", ".Last.lib")

is_reserved_name <- function(x) {
  x %in% reserved_names | grepl("^\\.\\.[0-9]+$", x) | startsWith(x, ".__")
}

empty_resolution <- function() {
  data.frame(name = character(), collection = character(), object = character(),
             kind = character(), binding = character(), stringsAsFactors = FALSE)
}

# Metadata-only name resolution across all registered collections.
# Precedence for a contested bare name: explicit preference, then the primary
# collection, otherwise "ambiguous". Suffix aliases must be globally unique.
resolve_names <- function(st) {
  rows <- lapply(st$collections, function(co) {
    o <- co$objects[co$objects$exposed & !is.na(co$objects$object), , drop = FALSE]
    if (!nrow(o)) return(NULL)
    r <- data.frame(name = o$object, collection = co$id, object = o$object,
                    kind = "original", stringsAsFactors = FALSE)
    if (!is.na(co$suffix)) {
      r <- rbind(r, data.frame(name = paste0(o$object, co$suffix), collection = co$id,
                               object = o$object, kind = "alias", stringsAsFactors = FALSE))
    }
    r
  })
  tab <- rbind_list(rows, empty_resolution()[, 1:4])
  if (!nrow(tab)) return(empty_resolution())

  bad <- tab[is_reserved_name(tab$name), , drop = FALSE]
  if (nrow(bad)) {
    pd_abort("Reserved binding name(s) cannot be exposed: ",
             paste0(bad$name, " (collection '", bad$collection, "')", collapse = ", "),
             ". Restrict the collection with `include`.", class = "projectData_reserved_name")
  }
  dup <- duplicated(tab$name) | duplicated(tab$name, fromLast = TRUE)
  clash <- tab[dup & tab$name %in% tab$name[tab$kind == "alias"], , drop = FALSE]
  if (nrow(clash)) {
    pd_abort("Exposed name collision involving a suffix alias: ",
             paste0("'", clash$name, "' (", clash$kind, ", collection '", clash$collection, "')",
                    collapse = "; "),
             ". Use a different `suffix`.", class = "projectData_name_collision")
  }

  tab$binding <- "bound"
  for (nm in unique(tab$name[duplicated(tab$name)])) {
    idx <- which(tab$name == nm)
    pref <- unname(st$preferences[nm])
    prim <- st$primary
    winner <- if (!is.na(pref) && pref %in% tab$collection[idx]) pref else
      if (!is.na(prim) && prim %in% tab$collection[idx]) prim else NA_character_
    tab$binding[idx] <- if (is.na(winner)) "ambiguous" else
      ifelse(tab$collection[idx] == winner, "bound", "not_bound")
  }
  tab
}

ambiguity_message <- function(name, res) {
  cands <- res$collection[res$name == name & res$binding == "ambiguous"]
  paste0("'", name, "' is ambiguous: it is provided by collections ",
         quote_names(cands), ". Resolve with data_prefer(\"", name, "\", \"",
         cands[1], "\"), designate a primary collection with data_primary(), use a ",
         "suffix alias if one was registered, or access it explicitly with ",
         "data_get(\"", name, "\", collection = \"", cands[1], "\").")
}

validate_state <- function(cand) {
  ids <- names(cand$collections)
  if (anyDuplicated(ids)) pd_abort("Duplicate collection ids.")
  if (!is.na(cand$primary) && !cand$primary %in% ids) {
    pd_abort("Primary collection '", cand$primary, "' is not attached.")
  }
  res <- resolve_names(cand)
  for (nm in names(cand$preferences)) {
    co <- cand$preferences[[nm]]
    ok <- any(res$name == nm & res$collection == co & res$kind == "original")
    if (!ok) {
      pd_abort("Preference for '", nm, "' names collection '", co,
               "', which does not expose an object '", nm, "'. Clear it with ",
               "data_prefer(\"", nm, "\", NULL) first.", class = "projectData_bad_preference")
    }
  }
  if (length(ids) && identical(cand$facade$on_mask, "error")) {
    m <- find_masks(planned_names(cand, res), cand$facade$env)
    if (nrow(m)) pd_abort(mask_message(m), class = "projectData_masking")
  }
  invisible(res)
}
