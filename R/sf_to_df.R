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
#'   single column or two separated columns
#' @param crs cCordinate reference system, Default: 4326
#' @param geometry Type of geometry to include in data frame. options include
#'   "drop", "wkt", "centroid", "point", Default: 'centroid'
#' @param coords Coordinate columns for input dataframe or output sf object (if
#'   geometry is 'centroid' or 'point') Default: c("lon", "lat")
#' @param keep_all If `FALSE`, drop all columns other than those named in
#'   coords, Default: `TRUE`
#' @param into If coords is a single column name with both longitude and
#'   latitude, `into` is used as the names of the new columns that coords is
#'   separated into. Passed to \code{\link[tidyr]{separate}}
#' @param sep If coords is a single column name with both longitude and
#'   latitude, `sep` is used as the separator between coordinate values. Passed
#'   to \code{\link[tidyr]{separate}}
#' @return `sf_to_df()` returns a data frame with geometry dropped or converted
#'   to wkt or coordinates for the centroid or point on surface; `df_to_sf()`
#'   returns a simple feature object with POINT geometry
#' @seealso \code{\link[sf]{st_coordinates}}
#' @rdname sf_to_df
#' @export
sf_to_df <- function(x,
                     crs = 4326,
                     coords = c("lon", "lat"),
                     geometry = "centroid",
                     keep_all = TRUE) {
  x <- st_transform_ext(x, crs = crs)

  x <- st_coords(x, geometry = geometry, coords = coords, keep_all = TRUE, drop = TRUE)

  return(as.data.frame(x))
}

#' @rdname sf_to_df
#' @name df_to_sf
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'   nc_df <- ggspatial::df_spatial(nc)
#'
#'   df_to_sf(nc_df, coords = c("x", "y"))
#'
#'   nc_df$xy <- paste(nc_df$x, nc_df$y, sep = ",")
#'
#'   df_to_sf(nc_df, coords = "xy", into = c("lon", "lat"))
#' }
#' }
#' @seealso
#'  \code{\link[ggspatial]{df_spatial}}
#'  \code{\link[sf]{st_as_sf}}
#' @export
#' @importFrom tidyr separate
#' @importFrom tidyselect all_of
#' @importFrom dplyr mutate across
#' @importFrom readr parse_number
#' @importFrom usethis ui_info
#' @importFrom sf st_transform st_as_sf
df_to_sf <- function(x,
                     crs = NULL,
                     coords = c("lon", "lat"),
                     into = NULL,
                     sep = ",",
                     rev = TRUE,
                     coords_crs = 4326,
                     remove_coords = FALSE) {
  if ((length(coords) == 1) && (length(into) == 2)) {
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

  if (!is.null(crs)) {
    x <- st_transform_ext(x = x, crs = crs, class = "sf")
  }

  return(x)
}

#' @rdname sf_to_df
#' @name df_to_sf
#' @param rev If TRUE, reverse c("lat", "lon") coords to c("lon", "lat"). check_coords only.
check_coords <- function(x = NULL, coords = NULL, rev = FALSE) {
  if (!is.null(x) && is.data.frame(x)) {
    data_names <- tolower(names(x))
    # FIXME: there may still be an issue with capitalization (at least with consistency)
    if ("lon" %in% data_names) {
      x_coords <- c("lon", "lat")
    } else if ("long" %in% data_names) {
      x_coords <- c("long", "lat")
    } else if ("longitude" %in% data_names) {
      x_coords <- c("longitude", "latitude")
    } else if ("y" %in% data_names) {
      x_coords <- c("y", "x")
    } else {
      x_coords <- NULL
    }

    if (!is.null(coords) && !is.null(x_coords) && !setequal(coords, x_coords)) {
      usethis::ui_warn("The provided coordinates do not appear to match the data.
                     Replacing coordinates with suggested values based on column names.")
      coords <- x_coords
    } else if (is.null(x_coords) && is.null(coords)) {
      usethis::ui_warn("Appropriate coordinate column names could not be determined based on the data provided.")
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
