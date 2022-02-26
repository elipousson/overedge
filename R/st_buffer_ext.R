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
#' @param dist_limits Numeric vector of any length (minimum and maximum values
#'   used as lower and upper limits on distance buffer). Units must match the
#'   provided units; defaults to NULL.
#' @param ... additional parameters passed to  \code{\link[sf]{st_buffer}}.
#' @export
#' @importFrom sf st_is_longlat st_crs st_transform st_bbox st_buffer
#' @importFrom units set_units
st_buffer_ext <- function(x,
                          dist = NULL,
                          diag_ratio = NULL,
                          unit = "meter",
                          dist_limits = NULL,
                          ...) {

  # If bbox, convert to sf
  if (check_bbox(x)) {
    x <- sf_bbox_to_sf(x)
  }

  # If dist is NULL and diag_ratio is NULL return x (with bbox converted to sf if no buffer applied)
  if (!is.null(dist) | !is.null(diag_ratio)) {

    # If longlat, save crs and transform to suggested crs
    is_lonlat <- sf::st_is_longlat(x)

    if (is_lonlat) {
      lonlat_crs <- sf::st_crs(x)
      x <- sf::st_transform(x, 3857)
    }

    if (check_class(dist, "units")) {
      unit <- as.character(units(dist)$numerator)
      dist <- as.numeric(dist)
    } else if (is.null(dist) && !is.null(diag_ratio)) {
      # Use the bbox diagonal distance to make proportional buffer distance
      dist <- sf_bbox_diagdist(sf::st_bbox(x)) * diag_ratio
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

    if (!is.null(dist_limits)) {
      dist_limits <-
        units::set_units(
          x = dist_limits,
          value = unit,
          mode = "standard"
        )
    }

    if (units_gdal == "meter") {
      dist <- units::set_units(
        x = dist,
        value = "meter"
      )

      if (!is.null(dist_limits)) {
        dist_limits <- units::set_units(
          x = dist_limits,
          value = "meter"
        )
      }
    } else if (units_gdal == "US_survey_foot") {
      dist <- units::set_units(
        x = dist,
        value = "US_survey_foot"
      )

      if (!is.null(dist_limits)) {
        dist_limits <- units::set_units(
          x = dist_limits,
          value = "US_survey_foot"
        )
      }
    }

    #   min_dist <- units::as_units(min(dist_limits), value = "US_survey_foot")
    #    max_dist <- units::as_units(max(dist_limits), value = "US_survey_foot")

    if (!is.null(dist_limits) && (length(dist_limits) >= 2) && check_class(dist_limits, "units")) {
      min_limit <- min(dist_limits)
      max_limit <- max(dist_limits)

      check_between_dist <-
        suppressWarnings(
          dplyr::between(
            dist,
            min_limit,
            max_limit
          )
        )

      if (check_between_dist) {
        usethis::ui_info("The buffer dist ({dist} {units_gdal}) is between the min/max distance limits.")
        dist <- dist_limits[[which.min(abs(dist_limits - dist))]]
        usethis::ui_info("Replacing with nearest distance limit ({dist} {units_gdal}).")
      } else if (dist < min_limit) {
        dist <- min_limit
        usethis::ui_info("Replacing buffer dist ({num_dist} {units_gdal}) with the minimum limit ({min_limit} {units_gdal}).")
      } else if (dist > max_limit) {
        dist <- max_limit
        usethis::ui_info("Replacing buffer dist ({num_dist} {units_gdal})with the maximum limit ({max_limit} {units_gdal}).")
      }
    }

    x <- sf::st_buffer(x = x, dist = dist, ...)

    if (is_lonlat) {
      x <- sf::st_transform(x, lonlat_crs)
    }
  }

  return(x)
}
