#' Convert between simple feature and data frame objects
#'
#' Helper function to convert a simple feature object to data frame by dropping
#' geometry, converting geometry to well known text, or (if the geometry type is
#' not POINT) getting coordinates for a centroid or point on surface. If an sfc
#' object is provided,the "drop" geometry option is not supported. check_coords
#' is a helper function used by df_to_sf to suggest the appropriate coordinate
#' column names based on the column names in the provided data frame.
#'
#' @param x A `sf` or `sfc` object or a data frame with lat/lon coordinates in a
#'   single column or two separated columns.
#' @param crs Cordinate reference system to return, Default: 4326 for [sf_to_df] and NULL for [df_to_sf]
#' @param coords_crs For [df_to_sf], coordinate reference system used by coordinates in data frame.
#' @param geometry Type of geometry to include in data frame. options include
#'   "drop", "wkt", "centroid", "point", Default: 'centroid'.
#' @param coords Coordinate columns for input dataframe or output sf object (if
#'   geometry is 'centroid' or 'point') Default: c("lon", "lat").
#' @param remove_coords For [df_to_sf], if `TRUE`, remove the coordinate columns after converting
#'   a data frame to simple feature object; defaults to `FALSE`.
#' @param keep_all If `FALSE`, drop all columns other than those named in
#'   coords, Default: `TRUE`.
#' @param into If coords is a single column name with both longitude and
#'   latitude, `into` is used as the names of the new columns that coords is
#'   separated into. Passed to [tidyr::separate].
#' @param sep If coords is a single column name with both longitude and
#'   latitude, `sep` is used as the separator between coordinate values. Passed
#'   to [tidyr::separate].
#' @return `sf_to_df()` returns a data frame with geometry dropped or converted
#'   to wkt or coordinates for the centroid or point on surface; `df_to_sf()`
#'   returns a simple feature object with POINT geometry.
#' @seealso \code{\link[sf]{st_coordinates}}
#' @example examples/sf_to_df.R
#' @rdname sf_to_df
#' @export
sf_to_df <- function(x,
                     crs = 4326,
                     coords = c("lon", "lat"),
                     geometry = "centroid",
                     keep_all = TRUE) {
  x <- st_transform_ext(x, crs = crs)
  x <- st_coords(x, geometry = geometry, coords = coords, keep_all = keep_all, drop = TRUE)
  return(x)
}

#' @rdname sf_to_df
#' @name df_to_sf
#' @seealso
#'  \code{\link[ggspatial]{df_spatial}}
#'  \code{\link[sf]{st_as_sf}}
#' @export
#' @importFrom sf st_sf st_geometry st_as_sf
#' @importFrom rlang has_length has_name
df_to_sf <- function(x,
                     crs = NULL,
                     coords = c("lon", "lat"),
                     coords_crs = 4326,
                     into = NULL,
                     sep = ",",
                     rev = TRUE,
                     remove_coords = FALSE) {
  if ((rlang::has_name(x, "geometry")) && is_sfc(x$geometry)) {
    x <- sf::st_as_sf(x)
  } else {
    if (rlang::has_length(coords, 1) && rlang::has_length(into, 2)) {
      x <- separate_coords(x = x, coords = coords, into = into, sep = sep)
      coords <- into
    } else {
      coords <- check_coords(x = x, coords = coords, rev = rev)
    }

    lon <- coords[[1]]
    lat <- coords[[2]]

    # Check that lat/lon are numeric
    if (!is.numeric(x[[lon]]) | !is.numeric(x[[lat]])) {
      x[[lon]] <- as.double(x[[lon]])
      x[[lat]] <- as.double(x[[lat]])
    }

    # Check for missing coordinates
    missing_coords <- is.na(x[[lon]] | x[[lat]])
    num_missing_coords <- sum(missing_coords)

    if (num_missing_coords > 0) {
      usethis::ui_info("Removing {num_missing_coords} rows with missing coordinates.")
      # Exclude rows with missing coordinates
      x <- x[!missing_coords, ]
    }

    x <-
      sf::st_as_sf(
        x,
        coords = c(lon, lat),
        agr = "constant",
        crs = coords_crs,
        stringsAsFactors = FALSE,
        remove = remove_coords
      )
  }

  x <- st_transform_ext(x = x, crs = crs, class = "sf")

  return(x)
}

#' Separate coordinates from a single combined column into two columns
#'
#' @noRd
#' @importFrom tidyr separate
#' @importFrom tidyselect all_of
#' @importFrom dplyr mutate across
#' @importFrom readr parse_number
separate_coords <- function(x, coords, into, sep) {
  into <- check_coords(x = NULL, coords = into)

  x <-
    tidyr::separate(
      x,
      col = tidyselect::all_of(coords),
      into = into,
      sep = sep
    )

  x <-
    dplyr::mutate(
      x,
      dplyr::across(
        .cols = tidyselect::all_of(into),
        ~ readr::parse_number(.x)
      )
    )

  return(x)
}

#' @rdname sf_to_df
#' @name df_to_sf
#' @param rev If TRUE, reverse c("lat", "lon") coords to c("lon", "lat"). check_coords only.
check_coords <- function(x = NULL, coords = NULL, rev = FALSE) {
  if (!is.null(x) && is.data.frame(x)) {
    # data_names <- tolower(names(x))
    # FIXME: there may still be an issue with capitalization (at least with consistency)
    if (rlang::has_name(x, "lon")) {
      x_coords <- c("lon", "lat")
    } else if (rlang::has_name(x, "long")) {
      x_coords <- c("long", "lat")
    } else if (rlang::has_name(x, "longitude")) {
      x_coords <- c("longitude", "latitude")
    } else if (rlang::has_name(x, "y")) {
      x_coords <- c("y", "x")
    } else {
      x_coords <- NULL
    }

    if (!is.null(coords) && !is.null(x_coords) && !setequal(coords, x_coords)) {
      usethis::ui_warn("The provided coordinates do not appear to match the data.
                     Replacing coordinates with suggested values based on column names.")
      coords <- x_coords
    } else if (is.null(x_coords) && is.null(coords)) {
      usethis::ui_warn("A pair of coordinate column names could not be determined based on the data provided.")
    }
  }

  if (is.null(coords)) {
    coords <- c("lon", "lat")
  } else {
    stopifnot(
      length(coords) == 2,
      is.character(coords) || is.numeric(coords)
    )
  }

  # FIXME: This automatic reversal needs to be documented
  if (rev && grepl("LAT|lat|Y|y", coords[1])) {
    coords <- rev(coords)
  }

  return(coords)
}
