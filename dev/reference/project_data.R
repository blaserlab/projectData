#' @title Activate Project Data
#' @description Use this to update, install and/or load project data.  Usual practice is to provide the path to a directory holding data package tarballs.  This function will find the newest version, compare that to the versions in the cache and used in the package and give you the newest version.  Alternatively, provide the path to a specific .tar.gz file to install and activate that one.
#'
#' If a specific version is requested, i.e. a specific .tar.gz file, and this version is already cached, it will be linked and not reinstalled.  If for some reason there are multiple hashes with the same version number (usually because a package was rebuilt without incrementing the version), then the latest hash of that version will be linked.
#'
#' This function accepts multiple paths, i.e. multiple independent data packages, in the form of a character vector of length >= 1.  After deciding which version to install based on the inputs, the function will load all of the data objects into a single environment called deconflicted.data.  The problem with loading multiple data packages into the same environment is that there may be name conflicts and objects get overridden.  The problem with keeping them in separate environments is that they are difficult to specify and access.  Here is how this function deals with these problems:
#'
#' * If `length(path) > 1`, the function will require a vector for the argument deconflict_string of the same length.  The first element of deconflict_string will be added as a suffix to the data object from the first package in path, etc.  For example if the first value of the argument deconflict_string is ".my.project.data", then all objects in the package will be suffixed with .my.project.data.
#' * Note that you will have to reference the object correctly in your code using the proper suffix.
#' * Also note that all of the elements of deconflict_string must be unique.  But an empty string, i.e. "",  is also a valid input which means that all of the names of the data objects from that package will be unchanged. This is helpful if you have a lot of code using one data package but at a later time decide you need to add a different data package.  Make the deconflict string `c("", ".my.new.data")` and you don't have to change any of your old code.
#' * Make sure you include a separator like . or _ but not a space as the first character of each element of deconflict_string.
#' * If only a single package is loaded, there will be no conflicts and by default, deconflict_string is set to "".
#'
#' As before, all data elements are loaded as promises which means that they are loaded into memory only when called.
#'
#' Since version 9211, this function also handles on-disk storage for monocle3 cds objects.  If such an object is detected within extdata, it will be loaded into the same deconflicted.data environment using the functions provided by monocle.
#'
#' @param path Path or vector of paths to data directory/ies.
#' @param deconflict_string Character vector used to disambiguate objects from packages in path, Default: ''
#' @return loads data as promises as a side effect
#' @seealso
#'  \code{\link[cli]{cli_abort}}, \code{\link[cli]{cli_alert}}
#'  \code{\link[readr]{read_delim}}, \code{\link[readr]{cols}}
#'  \code{\link[fs]{path}}, \code{\link[fs]{path_file}}
#'  \code{\link[purrr]{pmap}}, \code{\link[purrr]{map}}
#'  \code{\link[stringr]{str_detect}}, \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_remove}}, \code{\link[stringr]{str_replace}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{slice}}
#'  \code{\link[tibble]{as_tibble}}
#' @rdname project_data
#' @export
#' @import cli
#' @importFrom readr read_tsv cols
#' @import fs
#' @importFrom purrr pmap_chr walk
#' @importFrom stringr str_detect str_extract str_remove str_replace
#' @importFrom dplyr filter pull arrange slice slice_head
#' @importFrom tibble as_tibble
#' @import monocle3
project_data <- function(path, deconflict_string = "") {
  # catch_blasertemplates_root()
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  if (length(deconflict_string) != length(unique(deconflict_string))) {
    cli::cli_abort("All deconflict strings must be unique.")
  }
  if (length(path) > 1) {
    cli::cli_alert_warning("You are attempting to attach more than one data package.")
    cli::cli_alert_warning(
      "To prevent conflicts between items with the same name, each item\nwill be loaded with a unique suffix provided to the deconflict_string argument."
    )
  }
  if (length(path) != length(deconflict_string)) {
    cli::cli_abort("Please provide one unique string to identify each data package.")
  }
  cat <-
    readr::read_tsv(
      fs::path(
        Sys.getenv("BLASERTEMPLATES_CACHE_ROOT"),
        "package_catalog.tsv"
      ),
      col_types = readr::cols()
    )

  package <-
    purrr::pmap_chr(
      .l = list(path = path, ds = deconflict_string),
      .f = \(path, ds) {
        if (stringr::str_detect(string = path, pattern = ".tar.gz")) {
          # check if the requested version is available
          requested_version <-
            fs::path_file(path) |>
            stringr::str_extract("_.*") |>
            stringr::str_remove(".tar.gz") |>
            stringr::str_remove("_")
          datapackage_stem <- fs::path_file(path) |>
            stringr::str_remove("_.*")
          in_cache <- cat |>
            dplyr::filter(name == datapackage_stem) |>
            dplyr::pull(version) |>
            stringr::str_detect(requested_version) |>
            any()


          if (in_cache) {
            cli::cli_alert_warning(
              "{.emph {datapackage_stem}}, version {.emph {requested_version}} was found in the cache."
            )
            cli::cli_alert_warning("Newer versions may be available.")
            blaseRtemplates::install_one_package(
              package = datapackage_stem,
              how = "link_from_cache",
              which_version = requested_version
            )
            deconflict_datapkg(
              package = datapackage_stem,
              deconflict = ds,
              character.only = TRUE
            )
          } else {
            cli::cli_alert_warning("Installing {path}.  There may be newer versions available.")
            blaseRtemplates::install_one_package(package = path)
            blaseRtemplates::hash_n_cache()
            deconflict_datapkg(
              package = datapackage_stem,
              deconflict = ds,
              character.only = TRUE
            )
          }
        } else {
          latest_version <- file.info(list.files(path, full.names = T)) |>
            tibble::as_tibble(rownames = "file") |>
            dplyr::filter(stringr::str_detect(file, pattern = ".tar.gz")) |>
            dplyr::arrange(desc(mtime)) |>
            dplyr::slice(1) |>
            dplyr::pull(file) |>
            basename()
          datapackage_stem <-
            stringr::str_replace(latest_version, "_.*", "")
          latest_version_number <-
            stringr::str_replace(latest_version, "^.*_", "")
          latest_version_number <-
            stringr::str_replace(latest_version_number, ".tar.gz", "") |>
            as.package_version()

          # check if the newest version is available in blaseRtemplates cache

          latest_cached <- cat |>
            dplyr::filter(name == datapackage_stem) |>
            dplyr::arrange(desc(version), desc(date_time)) |>
            dplyr::slice_head(n = 1) |>
            dplyr::pull(version)

          in_cache <- FALSE
          cache_up_to_date <- FALSE
          project_up_to_date <- FALSE

          if (length(latest_cached) > 0) {
            in_cache <- TRUE
          }
          if (in_cache) {
            if (latest_cached >= latest_version_number) {
              cache_up_to_date <- TRUE
            }
          }
          if (possibly_packageVersion(datapackage_stem) == latest_version_number) {
            project_up_to_date <- TRUE
          }

          if (project_up_to_date) {
            cli::cli_alert_success("Your current version of {datapackage_stem} is up to date.")
            deconflict_datapkg(
              package = datapackage_stem,
              deconflict = ds,
              character.only = TRUE
            )
          } else {
            # check to see if cache is up to date and install from there if so
            if (cache_up_to_date) {
              blaseRtemplates::install_one_package(datapackage_stem, "link_from_cache")
              deconflict_datapkg(
                package = datapackage_stem,
                deconflict = ds,
                character.only = TRUE
              )
            } else {
              cli::cli_alert_info("Installing the latest version of {datapackage_stem}.")
              install_datapackage_2(path, latest_version)
              deconflict_datapkg(
                package = datapackage_stem,
                deconflict = ds,
                character.only = TRUE
              )
            }
          }
        }
        bpcells_dir <-
          fs::dir_ls(fs::path_package(datapackage_stem),
            recurse = 2,
            regexp = "bpcells_matrix_dir"
          )

        if (length(bpcells_dir) > 0) {
          cli::cli_alert("One or more on-disk cds objects have been detected.")
          cli::cli_inform(
            "These will be loaded into the deconflicted.data environment using monocle3::load_monocle_objects()."
          )
          cli::cli_inform("If provided, a deconflict string will be appended to the object name.")
          cli::cli_inform("For documentation of these objects, see the data package README file.")
          cli::cli_alert("Loading {.emph monocle3} and its dependencies.")
          suppressPackageStartupMessages(library("monocle3"))
          suppressPackageStartupMessages(library("BPCells"))

          for (i in seq_along(bpcells_dir)) {
            bp_path <- unlist(fs::path_split(bpcells_dir[i]))
            obj_name <- bp_path[length(bp_path) - 1]
            obj_name <- paste0(obj_name, ds)
            cli::cli_alert("Assigning {.emph {obj_name}} to the deconflicted.data environment.")
            assign(
              x = obj_name,
              value = monocle3::load_monocle_objects(
                fs::path_dir(bpcells_dir[i]),
                matrix_control = list(
                  matrix_class = "BPCells",
                  matrix_path = fs::path(tempdir(), "BPCells")
                )
              ),
              pos = "deconflicted.data"
            )
          }
        }
        datapackage_stem
      }
    )

  purrr::walk(
    .x = paste0("datasets:", package),
    .f = \(x) detach(name = x, character.only = TRUE)
  )
  purrr::walk(
    .x = paste0("package:", package),
    .f = \(x) detach(name = x, character.only = TRUE)
  )
}


  #' @importFrom stringr str_sub
  install_datapackage_2 <-
    function(path, latest_version) {
      if (stringr::str_sub(path, -1) == "/") {
        blaseRtemplates::install_one_package(paste0(path, latest_version))
        blaseRtemplates::hash_n_cache()
      } else {
        blaseRtemplates::install_one_package(paste0(path, "/", latest_version))
        blaseRtemplates::hash_n_cache()
      }

    }

  #' @importFrom purrr possibly
  possibly_packageVersion <-
    purrr::possibly(packageVersion, otherwise = "0.0.0.0000")

  #' @export
  #' @importFrom utils data
  #' @importFrom fs path path_package
  make_pointers <- function(package, str) {
    d <- utils::data(package = package)$results
    if (nrow(d) == 0)
      next
    d <- d[, "Item"]
    objName <- sub(" .*$", "", d)
    datName <- sub("^.*\\(", "", sub("\\)$", "", d))
    # get the dataset name in the environment list, datasets:<package>
    searchDataName <- paste0("datasets:", package)

    # if the dataset has been attached already, then detach it
    while (searchDataName %in% search())
      detach(searchDataName, character.only = TRUE)

    # get the package name in the environment list, package:<package>
    searchPkgName <- paste0("package:", package)

    # find where the package is attached
    pkgIndex <- match(searchPkgName, search(), nomatch = 1)

    # reattach the datasets:<package> environment behind the package
    pos <- pkgIndex + 1
    env <- attach(NULL, pos = pos, name = searchDataName)
    #?
    attr(env, "path") <- attr(as.environment(pkgIndex), "path")
    # for each data item in the data package
    for (i in seq_along(d)) {
      eval(substitute(
        delayedAssign(
          x = paste0(OBJ, STR),
          value =
            loadRData(fs::path(
              fs::path_package(PKG), "data", paste0(OBJ, ".rda")
            )),
          eval.env = globalenv(),
          assign.env = as.environment("deconflicted.data")
        ),
        list(
          OBJ = objName[i],
          SEARCHDATANAME = searchDataName,
          PKG = package,
          DAT = datName[i],
          STR = str
        )
      ))
    }
  }

  #' @export
  loadRData <- function(fileName) {
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }

  #' @export
  deconflict_datapkg <- function (package,
                                  deconflict,
                                  lib.loc = NULL,
                                  quietly = TRUE,
                                  character.only = TRUE,
                                  warn.conflicts = TRUE,
                                  reallyQuietly = TRUE,
                                  ...) {
    if (!character.only) {
      pkg <- substitute(package)
      if (!is.character(pkg))
        pkg <- deparse(pkg)
    }
    else
      pkg <- as.character(package)
    if (length(pkg) != 1)
      stop("only one package may be attached in any call")
    s0 <- search()
    oldWarn <- options(warn = -1)
    on.exit(options(oldWarn))
    OK <- if (reallyQuietly) {
      suppressMessages(require(
        package = pkg,
        lib.loc = lib.loc,
        quietly = TRUE,
        warn.conflicts = FALSE,
        character.only = TRUE
      ))
    }
    else {
      require(
        package = pkg,
        lib.loc = lib.loc,
        quietly = quietly,
        warn.conflicts = warn.conflicts,
        character.only = TRUE
      )
    }
    options(oldWarn)
    if (!OK) {
      warning("no valid package called ", sQuote(pkg), " found")
      return(invisible(FALSE))
    }
    if (!"deconflicted.data" %in% search()) {
      env2 <- attach(NULL, pos = 2L, name = "deconflicted.data")
      attr(env2, "path") <- attr(as.environment(2), "path")
    }
    package <- pkg
    if (file.exists(f <- system.file("data",
                                     package = package)) &&
        file.info(f)$isdir && !file.exists(file.path(f,
                                                     "Rdata.rds"))) {
      make_pointers(package = package, str = deconflict)
    }

    invisible(TRUE)
  }

  # search_for_
