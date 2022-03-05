#' Write or cache a simple feature object to a file
#'
#' @description
#' The write_sf_ext and write_sf_cache helper functions wrap the
#' \code{\link[sf]{write_sf}} function to provide some additional options
#' including consistent file naming with [make_filename()] and features
#' including:
#'
#' - If the data is not an sf object, optionally save as an RDS file.
#' - If filetype is "csv" or the filename ends in ".csv" the file is
#' automatically converted to a dataframe using [df_to_sf()]; if filetype is
#' "gsheet" the file is converted and turned into a new Google SHeet document
#' (if a Google account is authorized with the googlesheets4 package).
#' - If cache is `TRUE` use write_sf_cache to cache file after writing a copy to
#' the path provided.
#'
#' @param data `sf` object to write.
#' @param filename File name to use. If filename is provided and the data is an
#'   `sf` object make sure to include the file type, e.g. "data.gpkg" or
#'   "data.csv". Objects that are not simple features are written to RDS with
#'   `readr::write_rds()`.
#' @param data_dir cache data directory, defaults to
#'   \code{\link[rappdirs]{user_cache_dir}} when data_dir is NULL. (only used
#'   for write_sf_cache; default is used when cache = TRUE for write_sf_ext)
#' @param overwrite Logical. Default `FALSE`. If `TRUE`, overwrite any existing
#'   cached files that use the same file name.
#' @param filetype File type to write and cache, Default: 'geojson' for
#'   `write_sf_ext()`
#' @param cache If `TRUE`, write `sf` object to file in cache directory;
#'   defaults to `FALSE`.
#' @inheritParams make_filename
#' @inheritParams write_sf_cache
#' @seealso
#'  \code{\link[sf]{st_write}}
#' @export
#' @md
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
  filename <-
    make_filename(
      name = name,
      label = label,
      filetype = filetype,
      filename = filename,
      path = NULL,
      prefix = prefix,
      postfix = postfix
    )

  if (is.null(path)) {
    path <- filename
  } else {
    path <- file.path(path, filename)
  }

  write_sf_types(
    data = data,
    filename = filename,
    filetype = filetype,
    path = path
  )

  if (cache) {
    write_sf_cache(
      data = data,
      filename = filename,
      overwrite = overwrite
    )
  }
}

#' @rdname write_sf_ext
#' @name write_sf_cache
#' @export
#' @importFrom usethis ui_yeah ui_done ui_stop
#' @importFrom sf write_sf
#' @importFrom readr write_rds
write_sf_cache <- function(data,
                           data_dir = NULL,
                           overwrite = FALSE,
                           name = NULL,
                           label = NULL,
                           filetype = NULL,
                           filename = NULL,
                           prefix = NULL,
                           postfix = NULL) {
  data_dir <- data_dir(data_dir)

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

  data_dir_path <- file.path(data_dir, filename)

  if (filename %in% list.files(data_dir)) {
    if (!overwrite) {
      overwrite <- usethis::ui_yeah(
        "A file with the same name exists in {usethis::ui_path(data_dir)}
        Do you want to overwrite {usethis::ui_value(filename)}?"
      )
    }

    if (overwrite) {
      usethis::ui_done("Removing existing cached data.")
      file.remove(data_dir_path)
    } else {
      usethis::ui_stop("{usethis::ui_path(filename)} was not overwritten.")
    }
  }

  write_sf_types(
    data = date,
    filename = filename,
    path = data_dir_path,
    filetype = filetype
  )
}

#' @noRd
#' @importFrom usethis ui_done ui_yeah
#' @importFrom readr write_csv write_rds
#' @importFrom sf write_sf
#' @importFrom googlesheets4 gs4_create write_sheet
write_sf_types <- function(data, filename = NULL, path, filetype = NULL) {
  if (check_sf(data)) {
    usethis::ui_done("Writing {usethis::ui_path(path)}")

    if (grepl(".csv$", path) | (!is.null(filetype) && (filetype == "csv"))) {
      readr::write_csv(
        x = sf_to_df(data),
        file = path
      )
    } else if (!is.null(filetype) && (filetype == "gsheet")) {
      sheet <-
        googlesheets4::gs4_create(
          name = filename # FIXME: Using the path as the name may cause issues
        )

      googlesheets4::write_sheet(
        data = sf_to_df(data),
        ss = sheet,
        sheet = 1
      )
    } else {
      sf::write_sf(
        obj = data,
        dsn = path
      )
    }
  } else if (
    usethis::ui_yeah(
      "This data is not a simple feature object.
      Do you want to save the file as an RDS file?"
    )
  ) {

    # Remove file extension from path
    if (!is.null(filetype)) {
      path <-
        sub(
          pattern = paste0("\\.", filetype, "$"),
          replacement = "",
          x = path
        )
    } else {
      path <-
        sub(
          pattern = paste0("\\.[:alpha:]+$"),
          replacement = "",
          x = path
        )
    }

    usethis::ui_done("Writing {usethis::ui_path(path)}")

    readr::write_rds(
      x = data,
      file = path
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
#' @family read_write
#' @export
#' @importFrom checkmate check_directory_exists
#' @importFrom usethis ui_yeah ui_done ui_stop
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
    if (!is.null(label)) {
      filename <-
        str_prefix(
          prefix = label,
          string = name,
          clean = TRUE
        )
    } else {
      filename <- name
    }

    filename <-
      str_prefix(
        prefix = prefix,
        string = filename,
        postfix = postfix,
        clean = TRUE
      )

    if (!is.null(filetype)) {
      filetype <- paste0(".", filetype)
    }

    filename <-
      paste0(filename, filetype)
  }

  if (!is.null(path)) {
    if (!(checkmate::check_directory_exists(path))) {
      if (usethis::ui_yeah("The directory {usethis::ui_path(path)} does not exist.
                       Do you want to create a new directory at this location?")) {
        dir.create(path)
        usethis::ui_done("Directory created at {usethis::ui_path(path)}")
      } else {
        usethis::ui_stop("Please provide a different directory for {usethis::ui_value(filename)}.")
      }
    }

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
  stopifnot(
    is.character(prefix) || is.null(prefix),
    is.character(string) || is.null(string),
    is.character(postfix) || is.null(postfix)
  )

  if (!is.null(prefix)) {
    if ("date" %in% prefix) {
      prefix <- gsub("^x", "", janitor::make_clean_names(Sys.Date(), sep_out = "-"))
    } else if ("time" %in% prefix) {
      prefix <- gsub("^x", "", janitor::make_clean_names(Sys.time(), sep_out = "-"))
    } else if (!is.null(prefix) && clean) {
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
#' @importFrom usethis ui_yeah ui_done
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
