#' Format data using common approaches
#'
#' This function can apply the following common data cleaning tasks:
#'
#' - Applies [stringr::str_squish] and [stringr::str_trim] to all character columns
#' - Optionally replaces all character values of "" with NA values
#' - Optionally corrects UNIX formatted dates with 1970-01-01 origins
#' - Optionally renames variables by passing a named list of
#'
#' @param data A tibble or data frame object
#' @param var_names A named list following the format, list("New var name" =
#'   old_var_name), or a two column data frame with the first column being the
#'   new variable names and the second column being the old variable names;
#'   defaults to `NULL`.
#' @param replace_na A named list to pass to [tidyr::replace_na]; defaults to
#'   `NULL`.
#' @param replace_with_na A named list to pass to [naniar::replace_with_na];
#'   defaults to `NULL`.
#' @param replace_empty_char_with_na If `TRUE`, replace "" with NA using
#'   [naniar::replace_with_na_if], Default: `TRUE`
#' @param fix_date If `TRUE`, fix UNIX dates (common issue with dates from
#'   FeatureServer and MapServer sources) , Default: `TRUE`
#' @return Data with formatting functions applied.
#' @rdname format_data
#' @export
#' @importFrom dplyr mutate across rename
#' @importFrom stringr str_trim str_squish
#' @importFrom tibble deframe
#' @importFrom tidyr replace_na
format_data <- function(data,
                        var_names = NULL,
                        replace_na = NULL,
                        replace_with_na = NULL,
                        replace_empty_char_with_na = TRUE,
                        fix_date = TRUE) {
  data <-
    dplyr::mutate(
      data,
      dplyr::across(
        where(is.character),
        ~ stringr::str_trim(stringr::str_squish(.x))
      )
    )

  if (!is.null(var_names)) {
    # https://twitter.com/PipingHotData/status/1497014703473704965

    if (is.data.frame(var_names)) {
      var_names <-
        tibble::deframe(var_names)
    }

    data <-
      dplyr::rename(
        data,
        !!!var_names
      )
  }

  if (!is.null(replace_na)) {
    data <-
      tidyr::replace_na(data, replace = replace_na)
  }


  if (!is.null(replace_with_na) || replace_empty_char_with_na) {
    is_pkg_installed("naniar")

    if (!is.null(replace_with_na)) {
      data <-
        naniar::replace_with_na(
          data,
          replace = replace_with_na
        )
    }

    if (replace_empty_char_with_na) {
      data <-
        naniar::replace_with_na_if(
          data,
          is.character, ~ .x == ""
        )
    }
  }

  if (fix_date) {
    data <- fix_date(data)
  }

  return(data)
}

#' @name format_data
#' @rdname format_data
fix_date <- function(data) {
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::contains("date"),
      ~ as.POSIXct(.x / 1000, origin = "1970-01-01")
    )
  )
}

#' @name relocate_sf_col
#' @rdname format_data
#' @importFrom dplyr everything relocate all_of
relocate_sf_col <- function(x, .after = dplyr::everything()) {
  dplyr::relocate(
    x,
    dplyr::all_of(attributes(x)$sf_column),
    .after = .after
  )
}
