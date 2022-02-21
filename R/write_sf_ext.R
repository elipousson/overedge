#' Save simple features to files, cache features, or load features to the environment
#'
#' This function supports a range of options for both simple feature objects and named lists of simple feature objects.
#'
#' - If load is TRUE, named lists can be loaded
#'
#'
#' @param data sf object or a list of sf objects
#' @param filetype File type to write and cache, Default: 'geojson'
#' @param filename file name
#' @param data_dir Data directory to write files, Default: NULL
#' @param write If TRUE, write the object with write_sf or readr write_csv (after converting to df with sf_to_df); defaults to TRUE
#' @param load If TRUE, load sf objects to global environment; defaults to FALSE
#' @param cache If TRUE, write sf object(s) to file in cache directory; defaults to FALSE
#' @inheritParams make_filename
#' @seealso
#'  \code{\link[sf]{st_write}}
#' @rdname write_sf_ext
#' @export
#' @importFrom purrr discard walk
#' @importFrom sf write_sf
#' @importFrom glue glue
write_sf_ext <- function(data,
                         name = NULL,
                         label = NULL,
                         prefix = NULL,
                         postfix = NULL,
                         filename = NULL,
                         filetype = "geojson",
                         path = NULL,
                         cache = FALSE,
                         overwrite = FALSE) {

  # If data is sf object, write or cache it
  if (check_sf(data)) {
    filename <-
      make_filename(
        name = name,
        label = label,
        filetype = filetype,
        filename = filename,
        path = path,
        prefix = prefix,
        postfix = postfix
      )

    if (write) {
      if ((filetype == "csv") | grepl(".csv$", filename)) {
        readr::write_csv(
          x = sf_to_df(data),
          file = filename
        )
      } else {
        sf::write_sf(
          obj = data,
          dsn = filename
        )
      }
    }

    if (cache) {
      if (!is.null(path)) {
        filename <-
          sub(
            pattern = paste0("^", file.path(path, "")),
            replacement = "",
            x = filename
          )
      }

      cache_sf(
        data = data,
        filename = filename,
        overwrite = overwrite
      )
    }
  } else if (check_sf_list(data)) {
    data <-
      purrr::discard(
        data,
        ~ nrow(.x) == 0
      )

    if (load) {
      list2env(
        x = data,
        envir = globalenv()
      )
    }

    purrr::walk(
      data,
      ~ write_sf_ext(
        data = .x,
        name = names(.x),
        filetype = filetype,
        prefix = prefix,
        postfix = postfix,
        data_dir = data_dir,
        write = write,
        cache = cache
      )
    )
  }
}


#' Cache simple feature data
#'
#' Cache data to `rappdirs::user_cache_dir("overedge")` or other data directory.
#'
#' @param data Data to cache.
#' @param filename File name to use for cached file. Defaults to name of data.
#'   If the data is an sf object make sure to include the file type, e.g.
#'   "data.gpkg", supported by `sf::write_sf()`. All other data is written to
#'   rda with `readr::write_rds()`.
#' @param data_dir cache data directory, defaults to
#'   \code{\link[rappdirs]{user_cache_dir}} when data_dir is NULL
#' @param overwrite Logical. Default FALSE. If TRUE, overwrite any existing
#'   cached files that use the same file name.
#' @inheritParams make_filename
#' @export
#' @importFrom usethis ui_yeah ui_done ui_stop
#' @importFrom sf write_sf
#' @importFrom readr write_rds
cache_sf <- function(data = NULL,
                     data_dir = NULL,
                     overwrite = FALSE,
                     name = NULL,
                     label = NULL,
                     filetype = NULL,
                     filename = NULL,
                     prefix = NULL,
                     postfix = NULL) {

  data_dir <- overedge_data_dir(data_dir)

  filename <-
    make_filename(
      name = name,
      label = label,
      filetype = filetype,
      filename = filename,
      prefix = prefix,
      postfix = postfix,
      path = NULL
      )

  data_path <- file.path(data_dir, filename)

  if (filename %in% list.files(data_dir)) {
    if (!overwrite) {
      overwrite <- usethis::ui_yeah(
        "A file with the same name exists in {usethis::ui_path(data_dir)}
        Do you want to overwrite {usethis::ui_value(filename)}?"
      )
    }

    if (overwrite) {
      usethis::ui_done("Removing existing cached data.")
      file.remove(data_path)
    } else {
      usethis::ui_stop("{usethis::ui_path(filename)} was not overwritten.")
    }
  }

  if (check_sf(data)) {
    usethis::ui_done("Writing {usethis::ui_path(data_path)}")

    sf::write_sf(
      obj = data,
      dsn = data_path
    )
  } else {
    cache_rds <- usethis::ui_yeah(
      "This data is not a simple feature object.
      Do you want to cache the file as an RDS file?"
    )

    if (!is.null(filetype)) {
      data_path <-
        sub(
          pattern = paste0(".", filetype, "$"),
          replacement = "",
          x = data_path
        )
    }

    usethis::ui_done("Writing {usethis::ui_path(data_path)}")

    if (cache_rds) {
      readr::write_rds(
        x = data,
        file = data_path
      )
    }
  }
}


#' @param data named list of sf objects
#' @noRd
load_sf_list <- function(data) {
  if (check_sf_list(data)) {
    list2env(
      x = data,
      envir = globalenv()
    )
  }
}

#' Make file name and path with optional label, prefix, or postfix
#'
#' A helper function to create consistent file names for plots created with
#' [ggsave_ext()] or data files exported with [write_sf_ext()].
#'
#' @param name Name to make file name converted to snake case with
#'   \code{\link[janitor]{make_clean_case}}, e.g. "Residential zoning map"
#'   becomes "residential_zoning_map"
#' @param label Label to combine with name converted to snake case with
#'   \code{\link[janitor]{make_clean_case}}. The label is designed to identify
#'   the area or other shared characteristics across multiple data files, maps,
#'   or plots.
#' @param filetype File type or extension.
#' @param filename File name; if file name is `NULL`, name and file type are both
#'   required.
#' @param path Path to file or data directory.
#' @param prefix File name prefix. "date" adds a date prefix, "time" adds a
#'   date/time prefix; defaults to `NULL`.
#' @param postfix File name postfix; defaults to `NULL`.
#' @export
#' @importFrom checkmate check_directory_exists
#' @importFrom glue glue
#' @importFrom janitor make_clean_names
make_filename <- function(name = NULL,
                          label = NULL,
                          filetype = NULL,
                          filename = NULL,
                          path = NULL,
                          prefix = NULL,
                          postfix = NULL) {
  stopifnot(
    is.character(name) || is.character(filename)
  )

  if (is.null(filename)) {

    filename <-
      str_prefix(prefix = c(prefix, label),
                 string = name,
                 postfix = postfix,
                 clean = TRUE)

    if (!is.null(filetype)) {
      filetype <- paste0(".", filetype)
    }

    filename <-
      paste0(filename, filetype)
  }

  if (!is.null(path)) {
    checkmate::check_directory_exists(path)
    filename <- file.path(path, filename)
  }

  return(filename)
}

#' Add prefix and postfix to string
#'
#' Prefix and postfix can include more than one value that are added in the same
#' order provided. String must be a single character string.
#'
#' @param prefix Character string or character vector to add to string parameter
#'   as a prefix.
#' @param string A single string that the attach prefix or postfix is added to.
#' @param postfix Character string or character vector to add to string
#'   parameter as a postfix.
#' @param sep Separator character passed as the collapse parameter of [paste()].
#' @param clean If `TRUE`, prefix, postfix, and string are all converted to
#'   snake case with \code{\link[janitor]{make_clean_names()}}.
#' @noRd
#' @importFrom janitor make_clean_names
str_prefix <- function(prefix = NULL, string = NULL, postfix = NULL, sep = "_", clean = TRUE) {

  if (!is.null(prefix)) {
    if ("date" %in% prefix) {
      prefix <- gsub("^x", "", janitor::make_clean_names(Sys.Date(), sep_out = "-"))
    } else if ("time" %in% prefix) {
      prefix <- gsub("^x", "", janitor::make_clean_names(Sys.time(), sep_out = "-"))
    } else if (is.character(prefix) && clean) {
      prefix <- janitor::make_clean_names(prefix)
    }
  }

  if (clean) {
    string <- janitor::make_clean_names(string)
  }

  string <- paste(c(prefix, string), collapse = sep)

  if (!is.null(postfix)) {
    if (clean) {
      postfix <- janitor::make_clean_names(postfix)
    }
    string <- paste(c(string, postfix), collapse = sep)
  }

  string <- gsub("_{2}", "_", string)

  return(string)
}

#' Check existence of data directory and create if nonexistent
#'
#' @param path Path to directory to use as data directory.
#' @noRd
#' @importFrom rappdirs user_cache_dir
#' @importFrom checkmate test_directory_exists
data_dir <- function(path = NULL) {
  if (is.null(path)) {
    path <- rappdirs::user_cache_dir("overedge")
  }

  if (!checkmate::test_directory_exists(path)) {
    if (usethis::ui_yeah("The data directory {usethis::ui_path(path)} does not exist.
                         Do you want to create a directory at this location?")) {
      dir.create(path = path)
      usethis::ui_done("New directory created at {usethis::ui_path(data_path)}")
    }
  }

  path
}
