#' Format data frames and simple features using common approaches
#'
#' This function can apply the following common data cleaning tasks:
#'
#' - Applies [stringr::str_squish] and [stringr::str_trim] to all character columns
#' - Optionally replaces all character values of "" with NA values
#' - Optionally corrects UNIX formatted dates with 1970-01-01 origins
#' - Optionally renames variables by passing a named list of variables
#'
#' @details Bind columns:
#'
#'  - [bind_address_col] bind a provided value for city, county, and state to a
#'  data frame (to supplement address data with consistent values for these
#'  variables)
#' - [bind_block_col] requires a data frame with columns named "bldg_num",
#' "street_dir_prefix", "street_name", and "street_type" and binds derived
#' values for whether a building is on the even or odd side of a block and
#' create a block number (street segment), and block face (street segment side)
#' identifier.
#'  - [bind_boundary_col] uses [sf::st_join] to assign simple feature data to an
#'  enclosing polygon.
#'
#' @details Simple feature only functions:
#'
#' - [rename_sf_col]
#' - [relocate_sf_col]
#' - [bind_boundary_col]
#'
#' @param x A tibble or data frame object
#' @param var_names A named list following the format, `list("New var name" = old_var_name)`, or a two column data frame with the first column being the
#'   new variable names and the second column being the old variable names;
#'   defaults to `NULL`.
#' @param clean_names If `TRUE`, pass data frame to [janitor::clean_names]; defaults to `TRUE`.
#' @param replace_na_with A named list to pass to [tidyr::replace_na]; defaults to
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
                        replace_na_with = NULL,
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
    x <- rename_with_xwalk(x, xwalk = var_names)
  }

  if (clean_names) {
    x <- janitor::clean_names(x, "snake")
  }

  if (!is.null(replace_na_with)) {
    x <-
      tidyr::replace_na(x, replace = replace_na_with)
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

#' @name rename_with_xwalk
#' @rdname format_data
#' @param xwalk a data frame with two columns using the first column as name and
#'   the second column as value; or a named list. The existing names of x must
#'   be the values and the new names must be the names.
#' @export
#' @importFrom tibble deframe
#' @importFrom rlang is_named
#' @importFrom dplyr rename
rename_with_xwalk <- function(x, xwalk = NULL) {

  # From https://twitter.com/PipingHotData/status/1497014703473704965

  if (is.data.frame(xwalk) && (ncol(xwalk) == 2)) {
    xwalk <-
      tibble::deframe(xwalk)
  }

  stopifnot(
    rlang::is_named(xwalk) && is.list(xwalk)
  )

  x <-
    dplyr::rename(
      x,
      !!!xwalk
    )

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

#' @name bind_block_col
#' @rdname format_data
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr mutate if_else
bind_block_col <- function(x,
                           bldg_num = "bldg_num",
                           street_dir_prefix = "street_dir_prefix",
                           street_name = "street_name",
                           street_suffix = "street_type") {
  stopifnot(
    rlang::has_name(x, c(bldg_num, street_dir_prefix, street, street_suffix))
  )

  dplyr::mutate(
    x,
    block_num = floor({{ bldg_num }} / 100) * 100,
    bldg_num_even_odd = dplyr::if_else(({{ bldg_num }} %% 2) == 0, "Even", "Odd"),
    block_num_st = paste(block_num, {{ street_dir_prefix }}, {{ street }}, {{ street_suffix }}),
    block_face_st = paste(bldg_num_even_odd, {{ street_dir_prefix }}, {{ street }}, {{ street_suffix }})
  )
}

#' @param boundary An sf object with a column named "name" or a list of sf
#'   objects where all items in the list have a "name" column.
#' @name bind_boundary_col
#' @rdname format_data
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr rename select
#' @importFrom sf st_join
bind_boundary_col <- function(x, boundary = NULL, ...) {
  if (!is_sf_list(boundaries)) {
    boundary <- as_sf_list(boundary)
  }
  stopifnot(
    all(sapply(boundary, rlang::has_name, "name"))
  )

  for (nm in names(boundary)) {
    y <-
      dplyr::rename(
        dplyr::select(boundary[[nm]], name),
        "{nm}" := "name"
      )

    x <- has_same_name_col(x, col = nm)
    x <- sf::st_join(x, y, ...)
  }

  x <- relocate_sf_col(x)

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

  if (is_sf(x)) {
    x <- relocate_sf_col(x)
  }

  return(x)
}
