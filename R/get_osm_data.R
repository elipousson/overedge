#' Use osmdata to get Open Street Map data for a location
#'
#' Wraps \code{osmdata} functions.
#'
#' @param location A `sf`, `sfc`, or `bbox` object.
#' @param key Feature key for overpass API query.
#' @param value Value of the feature key; can be negated with an initial
#'   exclamation mark, `value = "!this"`, and can also be a vector, `value = c("this", "that")`.
#'   If `value = "all"` or if `key = "building"` the values passed to the
#'   osmdata package are from
#'   \code{\link[osmdata]{available_tags()}}.
#' @param geometry Geometry type to output ("polygons", "points", "lines",
#'   "multilines", or "multipolygons"); if multiple geometry types are needed
#'   set osmdata to `TRUE.` Default `NULL`.
#' @param crs Coordinate reference system for output data; if NULL, the data
#'   remains in the Open Street Map coordinate reference system 4326. Default:
#'   `NULL`.
#' @param osmdata If `TRUE` return a `osmdata` class object that includes the
#'   overpass API call, metadata including timestamp and version numbers, and
#'   all available geometry types; defaults to `FALSE`.
#' @inheritParams st_bbox_ext
#' @return A simple feature object with features using selected geometry type or
#'   an `osmdata` object with features from all geometry types.
#' @export
#' @importFrom osmdata available_tags opq osmdata_sf add_osm_feature unique_osmdata
#' @importFrom purrr pluck
#' @importFrom sf st_transform
#' @importFrom usethis ui_info
get_osm_data <- function(location = NULL,
                         key,
                         value = NULL,
                         dist = NULL,
                         diag_ratio = NULL,
                         unit = NULL,
                         asp = NULL,
                         crs = NULL,
                         geometry = NULL,
                         osmdata = FALSE) {
  osm_geometry <-
    c(
      "polygons",
      "points",
      "lines",
      "multilines",
      "multipolygons"
    )

  osm_geometry <-
    match.arg(
      geometry,
      osm_geometry
    )

  osm_crs <- 4326

  # Get adjusted bounding box if any adjustment variables provided
  bbox_osm <- st_bbox_ext(
    x = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    crs = osm_crs
  )

  if ((key == "building") && is.null(value)) {
    value <- osm_building_tags
  }

  if (value == "all") {
    value <- osmdata::available_tags(key)
  }

  query <- try(osmdata::opq(bbox = bbox_osm, timeout = 90),
    silent = TRUE
  )
  data <-
    osmdata::osmdata_sf(
      osmdata::add_osm_feature(query, key = key, value = value)
    )

  if (!osmdata) {
    data <-
      purrr::pluck(
        data,
        var = paste0("osm_", osm_geometry)
      )

    if (!is.null(crs)) {
      data <- sf::st_transform(data, crs)
    }
  } else {
    data <- osmdata::unique_osmdata(data)
    data
  }

  if (getOption("overedge.osm_attribution", TRUE)) {
    usethis::ui_info("Attribution is required when you use Open Street Map data.
                     See {usethis::ui_value('https://www.openstreetmap.org/copyright')} for more information on the Open Database Licence.")
    options("overedge.osm_attribution" = FALSE)
  }

  return(data)
}
