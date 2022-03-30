
#' Filter and/or crop data using a bounding box
#'
#' @noRd
#' @importFrom sf st_crop st_filter
bbox_filter <- function(data, bbox = NULL, join = NULL, crop = TRUE) {
  bbox <- st_transform_ext(x = bbox, crs = data)

  if (crop) {
    data <- suppressWarnings(sf::st_crop(data, bbox))
  } else {
    # If no cropping, filter with bbox
    bbox_sf <- sf_bbox_to_sf(bbox)

    if (!is.null(join)) {
      data <- sf::st_filter(x = data, y = bbox_sf, join = join)
    } else {
      data <- sf::st_filter(x = data, y = bbox_sf)
    }
  }

  return(data)
}

#' Filter and/or trim data using an simple feature location
#'
#' @noRd
#' @importFrom cli cli_warn
#' @importFrom sf st_filter
sf_filter <- function(data, location, join = NULL, trim = FALSE) {
  location <- st_transform_ext(x = location, crs = data)

  if (is_point(location) || is_multipoint(location)) {
    location <- st_buffer_ext(x = location, dist = 0.00000001)

    if (trim) {
      cli::cli_warn("location_filter does not support trim = TRUE for POINT or MULTIPOINT geometry.")
      trim <- FALSE
    }
  }

  if (trim) {
    data <- st_trim(x = data, y = location)
  } else {
    if (!is.null(join)) {
      data <- sf::st_filter(x = data, y = location, join = join)
    } else {
      data <- sf::st_filter(x = data, y = location)
    }
  }
}

#' Filter, crop, or trim data to a location
#'
#' Location can be:
#'
#' - A sf, bbox, or sfc object
#' - A U.S. state (name, abbreviation, or GeoID) or county (GeoID)
#' - An address
#'
#' @param data Data to filter by location.
#' @param location A sf, sfc, or bbox object or a character string that is an
#'   address, county GeoID, state name, abbreviation, or GeoID (dist parameters
#'   are ignored if location is a character string).
#' @inheritParams st_bbox_ext
#' @param crop  If `TRUE`, data is cropped to location or bounding box
#'   [sf::st_crop] adjusted by the `dist`, `diag_ratio`, and `asp` parameters
#'   provided. Default TRUE.
#' @param trim  If `TRUE`, data is trimmed to area with [sf::st_intersection].
#'   This option ignores any `dist`, `diag_ratio`, or `asp` parameters. Default
#'   `FALSE`.
#' @param ... Additional parameters; bbox (used instead of location or adjusted
#'   location), county and state (used with get_counties or get_states), join
#'   (passed to [sf::st_filter])
#' @export
location_filter <- function(data,
                            location = NULL,
                            dist = NULL,
                            diag_ratio = NULL,
                            asp = NULL,
                            unit = "meter",
                            crs = NULL,
                            trim = FALSE,
                            crop = TRUE,
                            ...) {
  stopifnot(
    is_sf(data, ext = TRUE)
  )

  params <- rlang::list2(...)

  types <- is_location_type(location)
  has_us_state_param <- any(rlang::has_name(params, c("state", "statefp", "abb")))
  has_us_county_param <- any(rlang::has_name(params, c("county", "geoid")))

  if (types$char) {
    if (types$state) {
      location <- get_state_location(location)
    } else if (types$county) {
      location <- get_county_location(location)
    } else {
      # FIXME: Could this be replaced with a call to make_features
      # Geocode the address
      location <-
        tidygeocoder::geo(
          address = location,
          long = "lon",
          lat = "lat",
          quiet = rlang::is_interactive()
        )

      # Convert address df to sf
      location <- df_to_sf(location, coords = c("lon", "lat"), crs = data)
    }

    types$sf <- TRUE
    types$sf_ext <- TRUE
  } else if (is.null(location) && (has_us_state_param || has_us_county_param)) {
    if (has_us_state_param) {
      location <- get_state_location(location = c(params$state, params$statefp, params$abb)[[1]])
    } else if (has_us_county_param) {
      location <- get_county_location(location = c(params$county, params$geoid)[[1]])
    }

    types$sf <- TRUE
    types$sf_ext <- TRUE
  }

  has_bbox_param <- rlang::has_name(params, "bbox")

  if (types$sf_ext && !has_bbox_param) {
    bbox <-
      st_bbox_ext(
        x = location,
        dist = dist,
        diag_ratio = diag_ratio,
        asp = asp,
        unit = units,
        crs = data,
        class = "bbox"
      )
  } else if (has_bbox_param) {
    bbox <- params$bbox
  } else {
    bbox <- NULL
  }

  is_lonlat <- sf::st_is_longlat(data)

  if (is_lonlat) {
    suppressMessages(sf::sf_use_s2(FALSE))
  }

  if (types$sf_ext && (trim || is.null(bbox))) {
    data <- sf_filter(data = data, location = location, join = params$join, trim = trim)
  } else if (!is.null(bbox)) {
    # Otherwise, match bbox crs to data
    data <- bbox_filter(data = data, bbox = bbox, join = params$join, crop = crop)
  }

  if (is_lonlat) {
    suppressMessages(sf::sf_use_s2(TRUE))
  }

  return(data)
}

#' Is location a sf object, a sf/sfc/bbox object, a character string, or a state or county name/id?
#'
#' @noRd
is_location_type <- function(location = NULL) {

  is_null_location <- is.null(location)

    type <-
      list(
        "null" = is_null_location,
        "sf" = !is_null_location && is_sf(location),
        "sf_ext" = !is_null_location && is_sf(location, ext = TRUE),
        "char" = !is_null_location && is.character(location)
      )

  if (!is_null_location && type$char) {
    c(
      type,
      list(
        "state" = (location %in% c(us_states$name, us_states$abb, us_states$statefp, as.integer(us_states$statefp))),
        "county" = (location %in% c(us_counties$name, us_counties$geoid, as.integer(us_counties$geoid)))
      )
    )
  } else {
    type
  }
}

#' Get simple feature for location if location is a state name, abbreviation, or FIPS code
#'
#' @noRd
get_state_location <- function(location, class = "sf") {
  stopifnot(
    is.character(location) || is.integer(location)
  )

  if (location %in% us_states$abb) {
    location <- get_states(abb = location, class = class)
  } else if (location %in% us_states$name) {
    location <- get_states(name = location, class = class)
  } else if (location %in% us_states$statefp) {
    # FIXME: What about numeric/integer locations?
    location <- get_states(geoid = location, class = class)
  }

  return(location)
}

#' Get simple feature for location if location is a county GeoID or county name
#'
#' @noRd
get_county_location <- function(location, class = "sf") {
  if (location %in% us_counties$geoid) {
    location <- get_counties(geoid = location, class = class)
  } else if (location %in% us_counties$name) {
    location <- get_counties(name = location, class = class)
  }

  return(location)
}
