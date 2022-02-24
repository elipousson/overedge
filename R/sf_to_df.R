#' Convert simple feature object to data frame with coordinates or data frame to
#' simple feature
#'
#' Helper function to convert a simple feature object to data frame by dropping
#' geometry, converting geometry to well known text, or (if the geometry type is
#' not POINT) getting coordinates for a centroid or point on surface. If an sfc
#' object is provided, [sf_to_sfc()] provides coordinates but the "drop"
#' geometry option is not supported.
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
#' @importFrom sf st_crs st_transform st_as_text st_as_sfc st_geometry_type
#'   st_centroid st_point_on_surface st_coordinates st_drop_geometry
#' @importFrom dplyr bind_cols select
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
sf_to_df <- function(x,
                     crs = 4326,
                     coords = c("lon", "lat"),
                     geometry = "centroid",
                     keep_all = TRUE) {
  if (!is.null(crs) && (sf::st_crs(crs) != sf::st_crs(x))) {
    x <- sf::st_transform(x, crs)
  }

  geometry <- match.arg(geometry, c("drop", "wkt", "centroid", "point"))
  x_coords <- NULL

  if (geometry == "wkt") {
    # Convert geometry to wkt
    x$wkt <- sf::st_as_text(sf::st_as_sfc(x))
  } else {
    # Convert to coordinates at centroid or as a point on surface
    # FIXME: This approach may be an issue if a sf object has mixed geometry
    geometry_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))

    if (geometry_type != "POINT") {
      if (geometry == "centroid") {
        x <- suppressWarnings(sf::st_centroid(x))
      } else if (geometry == "point") {
        x <- suppressMessages(sf::st_point_on_surface(x))
      }
    }

    x_coords <- as.data.frame(sf::st_coordinates(x))

    # FIXME: This automatic reversal needs to be documented
    if (grepl("lat|Y|y", coords[1])) {
      coords <- rev(coords)
    }

    x_coords <-
      dplyr::select(
        x_coords,
        "{coords[1]}" := .data$X,
        "{coords[2]}" := .data$Y
      )
  }


  if (check_sf(x)) {
    x <-
      dplyr::bind_cols(
        sf::st_drop_geometry(x),
        x_coords
      )
  } else {
    # Returning coordinates for sfc object
    x <- x_coords
    keep_all <- TRUE
  }

  if (!keep_all) {
    x <- dplyr::select(x, tidyselect::all_of(coords))
  }

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
                     crs = 4326,
                     coords = c("lon", "lat"),
                     into = NULL,
                     sep = ",") {

  if ((length(coords) == 1) && (length(into) == 2)) {
    if (is.null(into)) {
      into <- c("lon", "lat")
    }

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
  }

  # FIXME: This automatic reversal needs to be documented
  if (grepl("lat|Y|y", coords[1])) {
    coords <- rev(coords)
  }

  longitude <- coords[[1]]
  latitude <- coords[[2]]

  # Check that lat/lon are numeric
  if (!is.numeric(x[[longitude]]) | !is.numeric(x[[latitude]])) {
    x[[longitude]] <- as.double(x[[longitude]])
    x[[latitude]] <- as.double(x[[latitude]])
  }

  # Check for missing coordinates
  missing_coords <- is.na(x[[longitude]] | x[[latitude]])
  num_missing_coords <- sum(missing_coords)

  if (num_missing_coords > 0) {
    usethis::ui_info("Removing {num_missing_coords} rows with missing coordinates.")
    # Exclude rows with missing coordinates
    x <- x[!missing_coords, ]
  }

  x <-
    sf::st_transform(
      sf::st_as_sf(
        x,
        coords = c(longitude, latitude),
        agr = "constant",
        crs = 4326,
        stringsAsFactors = FALSE,
        remove = FALSE
      ),
      crs
    )

  return(x)
}
