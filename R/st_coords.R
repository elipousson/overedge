
#' Get coordinates for simple feature or bounding box object
#'
#' An extended version of st_coords that supports binding coordinates to the
#' object, optionally dropping the geometry, and returning wkt or a point on
#' surface (geometry = "surface point") instead of the centroid.
#'
#' @param x sf, bbox, or sfc object
#' @param coords Column names to use for coordinates in results, Default: NULL;
#'   which is set to c("lon", "lat") by [check_coords]
#' @param geometry geometry to use for coordinates "centroid", "surface point",
#'   or alternatively "wkt"; defaults to NULL ("centroid")
#' @param keep_all If `TRUE`, bind the coordinates columns to the provided object x,
#'   Default: `TRUE`.
#' @param crs Coordinate reference system to use for coordinates; defaults to `NULL`.
#' @param drop If `TRUE` and x is an sf object, drop the geometry Default: `TRUE`.
#' @rdname st_coords
#' @export
#' @importFrom sf st_as_text st_as_sfc st_point_on_surface st_coordinates
#'   st_drop_geometry
#' @importFrom dplyr select bind_cols
st_coords <- function(x, coords = NULL, geometry = NULL, crs = NULL, keep_all = TRUE, drop = TRUE) {
  geometry <- match.arg(geometry, c("centroid", "surface point", "wkt"))

  stopifnot(
    check_sf(x, ext = TRUE)
  )

  if (!is.null(crs)) {
    x_coords <- st_transform_ext(x = x, crs = crs)
  } else {
    x_coords <- x
  }

  if (geometry == "wkt") {
    # Convert geometry to wkt
    x$geowkt <- sf::st_as_text(sf::st_as_sfc(x_coords))
  } else {
    # Convert to coordinates at centroid or as a point on surface
    # FIXME: This approach may be an issue if a sf object has mixed geometry
    geometry_type <- st_geom_type(x_coords, ext = FALSE)

    if (geometry_type != "POINT") {
      if (geometry == "centroid") {
        # FIXME: Double check that this doesn't cause issues for sfc objects
        x_coords <- st_center(x_coords, ext = FALSE)
      } else if (geometry == "surface point") {
        x_coords <- suppressMessages(sf::st_point_on_surface(x_coords))
      }
    }

    x_coords <- as.data.frame(sf::st_coordinates(x_coords))
    coords <- check_coords(coords = coords)

    x_coords <-
      dplyr::select(
        x_coords,
        "{coords[1]}" := .data$X,
        "{coords[2]}" := .data$Y
      )
  }

  # If x is an sfc or keep_all = FALSE return coordinates
  if (!keep_all) {
    if (geometry == "wkt") {
      return(x$geowkt)
    } else {
      return(x_coords)
    }
  } else if (geometry != "wkt") {
    # FIXME: What if someone passes a sf object with lon/lat columns to x
    x <- dplyr::bind_cols(x, x_coords)
  }

  if (drop) {
    x <- sf::st_drop_geometry(x)
  }

  return(x)
}
