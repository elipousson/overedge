
#' Make file name and path with optional label, prefix, or postfix
#'
#' A helper function to create consistent file names for plots created with
#' [ggsave_ext()] or data files exported with [write_sf_ext()].
#'
#' @param name Name to make file name converted to snake case with
#'   [janitor::make_clean_names()], e.g. "Residential zoning map" becomes
#'   "residential_zoning_map"
#' @param label Label to combine with name converted to snake case with
#'   [janitor::make_clean_names()]. The label is designed to identify the area
#'   or other shared characteristics across multiple data files, maps, or plots.
#' @param filetype File type or extension.
#' @param filename File name; if file name is `NULL`, name and file type are
#'   both required.
#' @param path Path to file or data directory.
#' @param prefix File name prefix. "date" adds a date prefix, "time" adds a
#'   date/time prefix; defaults to `NULL`.
#' @param postfix File name postfix; defaults to `NULL`.
#' @param cache If TRUE, path is set to the overedge cache directory using
#'   [get_data_dir()]; defaults to `FALSE`.
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
                          postfix = NULL,
                          cache = FALSE) {
  stopifnot(
    is.character(name) || is.character(filename)
  )

  if (cache) {
    path <- get_data_dir(path = NULL)
  } else if (!is.null(path)) {
    path <- get_data_dir(path = path)
  }

  if (!is.null(filetype)) {
    filetype <- paste0(".", filetype)
  } else if (is.null(filetype) && !is.null(filename)) {
    filetype <- stringr::str_extract(filename, "(?<=\\.).+$")
    filename <- stringr::str_remove(filename, paste0("\\.", filetype, "$"))
  }

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
  }

  filename <-
    str_prefix(
      prefix = prefix,
      string = filename,
      postfix = postfix,
      clean = TRUE
    )

  filename <-
    paste0(filename, filetype)

  filename <- file.path(path, filename)

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
