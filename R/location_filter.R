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
  bbox_in_params <- (!is.null(params$bbox) && is_bbox(params$bbox))
  dist_in_params <- (!any(sapply(c(dist, diag_ratio, asp), is.null)))
  us_geo_in_params <- (!is.null(params$county) || !is.null(params$state))

  is_sf_location <- !is.null(location) && is_sf(location, ext = TRUE)
  is_address_location <- (is.character(location) || us_geo_in_params)

  if (bbox_in_params) {
    # Always use a bbox if provided
    bbox <- params$bbox
  } else {
    bbox <- NULL
  }

  if (!is_sf_location && is.character(location)) {
    if (is_address_location) {
      county_in_params <-
        !is.null(params$county) && !is.null(params$state)

      state_in_params <-
        !is.null(params$state)

      is_state_location <-
        (location %in% c(us_states$name, us_states$abb, us_states$statefp, us_states$geoid))

      is_county_location <-
        (location %in% us_counties$geoid)

      if (county_in_params || is_county_location) {
        # usethis::ui_info("location is a U.S. county GeoID.")
        if (county_in_params) {
          # FIXME: Update with process for using county and state parameters
        } else if (is_county_location) {
          if (location %in% us_counties$geoid) {
            location <- get_counties(geoid = location, class = "sf")
          }
        }
      } else if (state_in_params || is_state_location) {
        # usethis::ui_info("location is a U.S. state name, abbreviation, or GeoID.")
        if (state_in_params) {
          # FIXME: Update with process for using state parameters
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
        }
      } else {
        # usethis::ui_info("location is a possible address.")

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
  }

  if (dist_in_params && is_sf_location && is.null(bbox)) {
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
  }

  stopifnot(
    is.null(bbox) || is_bbox(bbox)
  )

  is_lonlat <- sf::st_is_longlat(data)

  if (is_lonlat) {
    suppressMessages(sf::sf_use_s2(FALSE))
  }

  if (is_sf_location && (trim || is.null(bbox))) {
    location <- st_transform_ext(x = location, crs = data)

    if (is_point(location) || is_multipoint(location)) {
      location <- st_buffer_ext(x = location, dist = 0.00000001)

      if (trim) {
        usethis::ui_warn("location_filter does not support trim = TRUE for POINT or MULTIPOINT geometry.")
        trim <- FALSE
      }
    }

    if (trim) {
      # If trim, match location crs to data
      if (dist_in_params) {
        usethis::ui_warn("location_filter ignores the dist, diag_ratio, and/or asp parameters if trim is TRUE.")
      }

      data <- st_trim(x = data, y = location)
    } else {
      if (!is.null(params$join)) {
        data <- sf::st_filter(x = data, y = location, join = params$join)
      } else {
        data <- sf::st_filter(x = data, y = location)
      }
    }
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
  }

  if (is_lonlat) {
    suppressMessages(sf::sf_use_s2(TRUE))
  }

  return(data)
}
