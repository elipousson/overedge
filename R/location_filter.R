#' Filter data by location
#'
#' @param data Data to filter by location.
#' @param location A sf, sfc, or bbox object or a character string that is an
#'   address, county GeoID, state name, abbreviation, or GeoID (dist parameters
#'   are ignored if location is a character string).
#' @param crop  If `TRUE`, data is cropped to location or bounding box
#'   [sf::st_crop] adjusted by the `dist`, `diag_ratio`, and `asp` parameters
#'   provided. Default TRUE.
#' @param trim  If `TRUE`, data is trimmed to area with [sf::st_intersection].
#'   This option ignores any `dist`, `diag_ratio`, or `asp` parameters. Default
#'   `FALSE`.
#' @inheritParams st_bbox_ext
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
  bbox_in_params <- (!is.null(params$bbox) && is_bbox(params$bbox))
  dist_in_params <- (!any(sapply(c(dist, diag_ratio, asp), is.null)))
  us_geo_in_params <- (!is.null(params$county) || !is.null(params$state))

  is_sf_location <- !is.null(location) && is_sf(location, ext = TRUE)
  is_address_location <- (is.character(location) || us_geo_in_params)

  if (bbox_in_params) {
    bbox <- params$bbox
  } else if (dist_in_params && is_sf_location) {
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
  } else if (!is_address_location) {
    bbox <- NULL
  } else if (is_address_location) {
    county_in_params <-
      !is.null(params$county) && !is.null(params$state)

    state_in_params <-
      !is.null(params$state)

    is_state_location <-
      (location %in% c(us_states$name, us_states$abb, us_states$statefp, us_states$geoid))

    is_county_location <-
      (location %in% us_counties$geoid)

    if (county_in_params) {
      # FIXME: Update with process for using county and state parameters
    } else if (state_in_params) {
      # FIXME: Update with process for using state parameters
    } else if (is_county_location) {
      if (location %in% us_counties$geoid) {
        location <- get_counties(geoid = location, class = "sf")
      }
    } else if (is_state_location) {
      if (location %in% us_states$name) {
        location <- get_states(name = location, class = "sf")
      } else if (location %in% us_states$abb) {
        location <- get_states(abb = location, class = "sf")
      } else if (location %in% us_states$statefp) {
        location <- get_states(statefp = location, class = "sf")
      } else if (location %in% us_states$geoid) {
        location <- get_states(geoid = location, class = "sf")
      }
    } else {
      # Geocode the address
      location <-
        tidygeocoder::geo(
          address = location,
          long = "lon",
          lat = "lat"
        )

      # Convert address df to sf
      location <- df_to_sf(location, coords = c("lon", "lat"), crs = data)
    }

    is_sf_location <- TRUE
  }

  if (is_sf_location) {
    location <- st_transform_ext(x = location, crs = data)
  }

  if (trim && is_sf_location) {
    # If trim, match location crs to data
    if (dist_in_params) {
      usethis::ui_warn("The dist, diag_ratio, and/or asp parameters are ignored if trim is TRUE.")
    }

    data <- st_erase(x = data, y = location, flip = TRUE)
  } else if (!is.null(bbox)) {
    # Otherwise, match bbox crs to data
    bbox <- st_transform_ext(x = bbox, crs = data)

    if (crop) {
      data <- suppressWarnings(sf::st_crop(data, bbox))
    } else {
      # If no cropping, filter with bbox
      bbox_sf <- sf_bbox_to_sf(bbox)

      if (!is.null(params$join)) {
        data <- sf::st_filter(x = data, y = bbox_sf, join = params$join)
      } else {
        data <- sf::st_filter(x = data, y = bbox_sf)
      }
    }
  } else if (is_sf_location) {
    data <- sf::st_filter(x = data, y = location)
  }

  return(data)
}
