#' @importFrom sf st_bbox st_distance st_point
st_diag_dist <- function(x) {
  bbox <- sf::st_bbox(x)

  sf::st_distance(
    sf::st_point(
      c(bbox[["xmin"]], bbox[["ymin"]])
    ),
    sf::st_point(
      c(bbox[["xmax"]], bbox[["ymax"]])
    )
  )
}

#' Get buffered sf or bbox object
#'
#' Return an sf object with a buffer based on `dist` or a proportion of the
#' diagonal distance defined by `diag_ratio`.
#'
#' @param x sf or bbox object.
#' @param dist buffer distance in units. Optional.
#' @param diag_ratio ratio to set map extent based diagonal distance of area's
#'   bounding box. Ignored when \code{dist} is provided.
#' @param unit Units passed to `units::set_units()`.
#'   Default "metre" Must match units of the CRS for the provided object.
#' @param ... additional parameters passed to  \code{\link[sf]{st_buffer}}.
#' @export
#' @importFrom sf st_crs st_is_longlat st_buffer
#' @importFrom usethis ui_warn
#' @importFrom checkmate assert_number
#' @importFrom units set_units
st_buffer_ext <- function(x,
                          dist = NULL,
                          diag_ratio = NULL,
                          unit = "metre",
                          ...) {
  if (checkmate::test_class(x, "bbox")) {
    x <- sf_bbox_to_sf(x)
  }

  crs <- sf::st_crs(x)

  if (is.null(unit)) {
    unit <- crs$units_gdal
  } else if ((unit != crs$units_gdal)) {
    usethis::ui_warn("`x` has a coordinate reference system ({crs[[1]]}) that requires `unit` to be set to {crs$units_gdal}.")
    unit <- crs$units_gdal
  }

  # If dist is NULL
  if (is.null(dist) && !is.null(diag_ratio)) {
    # Use the bbox diagonal distance to make proportional buffer distance
    dist <- st_diag_dist(x) * diag_ratio
  } else if (is.null(dist)) {
    dist <- 0
  }

  checkmate::assert_number(dist)

  # Skip set_units if x is lat/lon
  if (sf::st_is_longlat(x)) {
    dist <-
      units::set_units(
        dist,
        value = unit,
        mode = "standard"
      )
  }

  sf::st_buffer(x = x, dist = dist, ...)
}
