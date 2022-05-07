#' Format data using common approaches
#'
#' This function can apply the following common data cleaning tasks:
#'
#' - Applies [stringr::str_squish] and [stringr::str_trim] to all character columns
#' - Optionally replaces all character values of "" with NA values
#' - Optionally corrects UNIX formatted dates with 1970-01-01 origins
#' - Optionally renames variables by passing a named list of variables
#'
#'
#' @param x A tibble or data frame object
#' @param var_names A named list following the format, `list("New var name" = old_var_name)`, or a two column data frame with the first column being the
#'   new variable names and the second column being the old variable names;
#'   defaults to `NULL`.
#' @param clean_names If `TRUE`, pass data frame to [janitor::clean_names]; defaults to `TRUE`.
#' @param replace_na A named list to pass to [tidyr::replace_na]; defaults to
#'   `NULL`.
#' @param replace_with_na A named list to pass to [naniar::replace_with_na];
#'   defaults to `NULL`.
#' @param replace_empty_char_with_na If `TRUE`, replace "" with `NA` using
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
format_data <- function(x,
                        var_names = NULL,
                        clean_names = TRUE,
                        replace_na = NULL,
                        replace_with_na = NULL,
                        replace_empty_char_with_na = TRUE,
                        fix_date = TRUE) {
  x <-
    dplyr::mutate(
      x,
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

    x <-
      dplyr::rename(
        x,
        !!!var_names
      )
  }

  if (clean_names) {
    x <- janitor::clean_names(x, "snake")
  }

  if (!is.null(replace_na)) {
    x <-
      tidyr::replace_na(x, replace = replace_na)
  }


  if (!is.null(replace_with_na) || replace_empty_char_with_na) {
    is_pkg_installed("naniar")

    if (!is.null(replace_with_na)) {
      x <-
        naniar::replace_with_na(
          x,
          replace = replace_with_na
        )
    }

    if (replace_empty_char_with_na) {
      x <-
        naniar::replace_with_na_if(
          x,
          is.character, ~ .x == ""
        )
    }
  }

  if (fix_date) {
    x <- fix_date(x)
  }

  return(x)
}

#' @name format_data
#' @rdname format_data
#' @export
#' @importFrom dplyr mutate across contains
fix_date <- function(x) {
  dplyr::mutate(
    x,
    dplyr::across(
      dplyr::contains("date"),
      ~ as.POSIXct(.x / 1000, origin = "1970-01-01")
    )
  )
}

#' @name relocate_sf_col
#' @rdname format_data
#' @param .after The location to place sf column after; defaults to
#'   [dplyr::everything].
#' @export
#' @importFrom dplyr everything relocate all_of
relocate_sf_col <- function(x, .after = dplyr::everything()) {
  dplyr::relocate(
    x,
    dplyr::all_of(attributes(x)$sf_column),
    .after = .after
  )
}


#' @name rename_sf_col
#' @rdname format_data
#' @param sf_col Name to use for the sf column after renaming; defaults to "geometry".
#' @export
#' @importFrom dplyr everything relocate all_of
rename_sf_col <- function(x, sf_col = "geometry") {
  dplyr::rename(
    x,
    "{sf_col}" := dplyr::all_of(attributes(x)$sf_column)
  )
}

#' @name bind_address_col
#' @rdname format_data
#' @param city,county,state City, county, and state to bind to data frame or
#'   `sf` object.
#' @export
#' @importFrom dplyr mutate
bind_address_col <- function(x, city = NULL, county = NULL, state = NULL) {
  if (!is.null(city)) {
    x <- has_same_name_col(x, col = "city")

    x <-
      dplyr::mutate(
        x,
        city = city
      )
  }

  if (!is.null(county)) {
    x <- has_same_name_col(x, col = "county")

    x <-
      dplyr::mutate(
        x,
        county = county
      )
  }

  if (!is.null(state)) {
    x <- has_same_name_col(x, col = "state")

    x <-
      dplyr::mutate(
        x,
        state = state
      )
  }

  return(x)
}


#' @name bind_units_col
#' @rdname format_data
#' @param y Vector of numeric or units values to bind to x.
#' @param units Units to use for y (if numeric) or convert to (if y is units
#'   class); defaults to `NULL`.
#' @param drop If `TRUE`, apply the [units::drop_units] function to the column
#'   with units class values and return numeric values instead; defaults to
#'   `FALSE`.
#' @param keep_all If `FALSE`, keep all columns. If `FALSE`, return only the
#'   named .id column.
#' @param .id Name to use for vector of units provided to "y" parameter, when
#'   "y" is bound to the "x" data frame or tibble as a new column.
#' @export
#' @importFrom units drop_units
#' @importFrom dplyr bind_cols
bind_units_col <- function(x, y, units = NULL, drop = FALSE, keep_all = TRUE, .id = NULL) {
  x <- has_same_name_col(x, col = .id)

  if (!is.null(units)) {
    if (units %in% c(dist_unit_options, area_unit_options)) {
      y <-
        convert_dist_units(
          dist = y,
          from = get_dist_units(y),
          to = units
        )
    }
  }

  if (drop) {
    y <- units::drop_units(y)
  }

  if (!keep_all) {
    return(y)
  }

  x <-
    dplyr::bind_cols(
      x,
      "{.id}" := y
    )

  return(relocate_sf_col(x))
}
