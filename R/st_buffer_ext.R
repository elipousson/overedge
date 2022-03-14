#' Buffer a simple feature or bounding box object
#'
#' Return an sf object with a buffer based on `dist` or a proportion of the
#' diagonal distance defined by `diag_ratio`. If x uses geographic coordinates,
#' the coordinate reference system is transformed into EPSG:3857 and then transformed back into the
#' original CRS after the buffer has been applied.
#'
#' st_edge is a variation on st_buffer_ext where dist or diag_ratio is used to
#' define the width of the edge to return either outside the existing geometry
#' (for positive dist values) or inside the existing geometry (for negative dist
#' values).
#'
#' @param x sf or bbox object.
#' @param dist buffer distance in units. Optional.
#' @param diag_ratio ratio of diagonal distance of area's bounding box used as
#'   buffer distance. e.g. if the diagonal distance is 3000 meters and the
#'   "diag_ratio = 0.1"  a 300 meter will be used. Ignored when \code{dist} is
#'   provided.
#' @param unit Units for buffer. Supported options include "meter", "foot",
#'   "kilometer", and "mile", "nautical mile" Common abbreviations (e.g. "km"
#'   instead of "kilometer") are also supported. Distance in units is converted
#'   to units matching GDAL units for x; defaults to "meter"
#' @param dist_limits Numeric vector of any length (minimum and maximum values
#'   used as lower and upper limits on distance buffer). Units must match the
#'   provided units; defaults to NULL.
#' @param ... additional parameters passed to  \code{\link[sf]{st_buffer}}.
#' @export
#' @importFrom sf st_is_longlat st_crs st_transform st_bbox st_buffer
#' @importFrom units set_units drop_units
st_buffer_ext <- function(x,
                          dist = NULL,
                          diag_ratio = NULL,
                          unit = "meter",
                          dist_limits = NULL,
                          ...) {

  # If bbox, convert to sf
  x <- as_sf(x)

  # If dist is NULL and diag_ratio is NULL return x (with bbox converted to sf if no buffer applied)
  if (!is.null(dist) | !is.null(diag_ratio)) {

    # If longlat, save crs and transform to suggested crs
    is_lonlat <- sf::st_is_longlat(x)

    if (is_lonlat) {
      lonlat_crs <- sf::st_crs(x)
      x <- sf::st_transform(x, 3857)
    }

    # Get crs and rename gdal units to match options for set_units
    crs <- sf::st_crs(x)

    if (check_class(dist, "units")) {
      if (is.null(unit)) {
        unit <- as.character(units(dist)$numerator)
      }
      dist <- units::drop_units(dist)
    } else if (is.null(dist) && !is.null(diag_ratio)) {
      # Use the bbox diagonal distance to make proportional buffer distance
      dist <- sf_bbox_diagdist(sf::st_bbox(x), units = FALSE) * diag_ratio
    }

    dist <-
      convert_dist_units(dist = dist, from_unit = unit, to_unit = crs$units_gdal)

    if (!is.null(dist_limits)) {
      dist_limits <-
        convert_dist_units(dist = dist_limits, from_unit = unit, to_unit = crs$units_gdal)
    }

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

#' Convert distances between different units
#'
#'
#' @param dist Numeric or units
#' @param from_unit Existing unit for dist, Default: NULL. If dist is a units object, the numerator is used as from_unit
#' @param to_unit Unit to convert distance to, Default: 'meter'
#' @return OUTPUT_DESCRIPTION
#' @rdname convert_dist_units
#' @export
#' @importFrom units set_units
convert_dist_units <- function(dist,
                               from_unit = NULL,
                               to_unit = "meter") {
  dist_is_units <- check_class(dist, "units")

  stopifnot(
    is.numeric(dist) || dist_is_units
  )

  if (dist_is_units) {
    from_unit <- as.character(units(dist)$numerator)
  }

  if (!is.null(from_unit)) {
    from_unit <- match.arg(from_unit, dist_unit_options)

    if (!dist_is_units) {
      from_unit <- gsub(" ", "_", from_unit)
      dist <-
        units::set_units(
          x = dist,
          value = from_unit,
          mode = "standard"
        )
    }
  }

  if (!is.null(to_unit)) {
    to_unit <- gsub(" ", "_", to_unit)
    to_unit <- match.arg(to_unit, dist_unit_options)

    dist <-
      units::set_units(
        x = dist,
        value = to_unit,
        mode = "standard"
      )
  }

  return(dist)
}

#' @rdname st_buffer_ext
#' @name st_edge
#' @importFrom sf st_difference
st_edge <- function(x,
                    dist = NULL,
                    diag_ratio = NULL,
                    unit = "meter",
                    ...) {

  # If bbox, convert to sf
  x <- as_sf(x)

  x_dist <- st_buffer_ext(x, dist = dist, diag_ratio = diag_ratio, unit = unit, ...)

  if (dist > 0) {
    x <- suppressWarnings(sf::st_difference(x_dist, x))
  } else if (dist < 0) {
    x <- suppressWarnings(sf::st_difference(x, x_dist))
  }

  return(x)
}
