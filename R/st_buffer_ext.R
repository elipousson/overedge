#' @importFrom sf st_bbox st_distance st_point
#' @noRd
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

#' Buffer a simple feature or bounding box object
#'
#' Return an sf object with a buffer based on `dist` or a proportion of the
#' diagonal distance defined by `diag_ratio`. If x uses geographic coordinates,
#' the coordinate reference system is transformed into the crs returned by
#' \code{\link[crsuggest]{suggest_top_crs}} and then transformed back into the
#' original CRS after the buffer has been applied.
#'
#' @param x sf or bbox object.
#' @param dist buffer distance in units. Optional.
#' @param diag_ratio ratio of diagonal distance of area's bounding box used as
#'   buffer distance. e.g. if the diagonal distance is 3000 meters and the
#'   "diag_ratio = 0.1"  a 300 meter will be used. Ignored when \code{dist} is
#'   provided.
#' @param unit Units for buffer. Supported options include "meter", "foot",
#'   "kilometer", and "mile", "nautical mile" Common abbreviations (e.g. "km"
#'   instead of "kilometer") are also supported. Units are passed to
#'   `units::set_units()` and then converted to units matching GDAL; defaults to
#'   "meter"
#' @param ... additional parameters passed to  \code{\link[sf]{st_buffer}}.
#' @export
#' @importFrom checkmate test_class
#' @importFrom sf st_is_longlat st_crs st_transform st_buffer
#' @importFrom crsuggest suggest_top_crs
#' @importFrom units set_units
st_buffer_ext <- function(x,
                          dist = NULL,
                          diag_ratio = NULL,
                          unit = "meter",
                          ...) {


  # If bbox, convert to sf
  if (checkmate::test_class(x, "bbox")) {
    x <- sf_bbox_to_sf(x)
  }

  # If dist is NULL and diag_ratio is NULL return x (with bbox converted to sf if no buffer applied)
  if (!is.null(dist) | !is.null(diag_ratio)) {

    # If longlat, save crs and transform to suggested crs
    is_lonlat <- sf::st_is_longlat(x)

    if (is_lonlat) {
      lonlat_crs <- sf::st_crs(x)
      top_crs <- suppressMessages(crsuggest::suggest_top_crs(x))
      x <- sf::st_transform(x, top_crs)
    }

    if (is.null(dist) && !is.null(diag_ratio)) {
      # Use the bbox diagonal distance to make proportional buffer distance
      dist <- st_diag_dist(x) * diag_ratio
    }

    # Get crs and rename gdal units to match options for set_units
    crs <- sf::st_crs(x)

    if (!is.null(unit)) {
      unit <- gsub(" ", "_", unit)
    }

    units_gdal <-
      switch(crs$units_gdal,
        "US survey foot" = "US_survey_foot",
        "metre" = "meter",
        "meter" = "meter"
      )

    unit_options <-
      unique(
        c(
          units_gdal, "m", "metre", "meter", "meters", "km", "kilometer", "kilometers",
          "ft", "foot", "feet", "yard", "yards", "mi", "mile", "miles", "nautical_mile"
        )
      )

    # Match parameter units to permitted options
    unit <- match.arg(unit, unit_options)

    dist <-
      units::set_units(
        x = dist,
        value = unit,
        mode = "standard"
      )

    if (units_gdal == "meter") {
      dist <- units::set_units(
        x = dist,
        value = "meter"
      )
    } else if (units_gdal == "US_survey_foot") {
      dist <- units::set_units(
        x = dist,
        value = "US_survey_foot"
      )
    }

    x <- sf::st_buffer(x = x, dist = dist, ...)

    if (is_lonlat) {
      x <- sf::st_transform(x, lonlat_crs)
    }
  }

  return(x)
}
