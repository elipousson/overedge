
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
#' @inheritParams str_pad_digit
#' @family read_write
#' @export
make_filename <- function(name = NULL,
                          label = NULL,
                          filetype = NULL,
                          filename = NULL,
                          path = NULL,
                          prefix = NULL,
                          postfix = NULL,
                          cache = FALSE,
                          pad = "0") {
  stopifnot(
    is.character(name) || is.character(filename)
  )

  if (cache) {
    path <- get_data_dir(path = path)
  } else if (!is.null(path)) {
    path <- create_data_dir(path, create = TRUE)
  }

  if (!is.null(filetype)) {
    filetype <- paste0(".", filetype)
  } else if (!is.null(filename)) {
    filetype <- stringr::str_extract(filename, "(?<=\\.).+$")
    filename <- str_remove_filetype(filename, filetype)
  }

  if (is.null(filename)) {
    filename <- name

    if (!is.null(label)) {
      filename <-
        str_prefix(
          prefix = label,
          string = name,
          clean = TRUE
        )
    }
  }

  filename <-
    str_prefix(
      prefix = prefix,
      string = filename,
      postfix = postfix,
      clean = TRUE,
      pad = pad
    )

  filename <-
    paste0(filename, filetype)

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  return(filename)
}


#' Add file type to string
#'
#' @importFrom stringr str_detect
str_add_filetype <- function(x, filetype = NULL) {
  if (stringr::str_detect(x, "\\.")) {
    return(x)
  }

  paste0(x, ".", filetype)
}


#' Remove file type from string
#'
#' @param x String
#' @param filetype File type string
str_remove_filetype <- function(x = NULL, filetype = NULL) {
  if (!is.null(filetype)) {
    sub(
      pattern = paste0("\\.", filetype, "$"),
      replacement = "",
      x = x
    )
  } else {
    sub(
      pattern = paste0("\\.[:alpha:]+$"),
      replacement = "",
      x = x
    )
  }
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
#'   snake case with [janitor::make_clean_names()].
#' @inheritParams str_pad_digit
#' @importFrom janitor make_clean_names
str_prefix <- function(prefix = NULL, string = NULL, postfix = NULL, sep = "_", clean = TRUE, pad = "0", width = NULL) {
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
    } else if (clean) {
      prefix <- janitor::make_clean_names(prefix)
    }
  }

  if (clean) {
    string <- janitor::make_clean_names(string)
  }

  string <- str_pad_digit(string, pad = pad, width = width)

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

#' Pad a string with digits
#'
#' @param pad Single padding character added to digits in string; defaults to
#'   "0"
#' @inheritParams stringr::str_pad
#' @importFrom stringr str_length str_pad str_replace
str_pad_digit <- function(string, pad = "0", side = "left", width = NULL) {
  digit_string <-
    str_extract_digit(string)

  if (is.null(width)) {
    width <-
      max(stringr::str_length(digit_string))
  }

  digit_string <-
    stringr::str_pad(
      digit_string,
      pad = pad,
      width = width
    )

  stringr::str_replace(
    string,
    pattern = "[:digit:]",
    replacement = digit_string
  )
}

#' Extract digits from a string
#'
#' @inheritParams stringr::str_extract
#' @noRd
#' @importFrom stringr str_extract
str_extract_digit <- function(string) {
  stringr::str_extract(string, "[:digit:]")
}
