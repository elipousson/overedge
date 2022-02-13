#' Get Open Street Map features for location
#'
#' Wraps \code{osmdata} functions.
#'
#' @param location sf, sfc, or bbox object
#' @param key feature key for overpass API query.
#' @param value value of feature key; can be negated with an initial exclamation
#'   mark, `value = "!this"`, and can also be a vector, `value = c ("this",
#'   "that")`. If `value = "all"` or if `key = "building"` the values passed to
#'   osmdata are from `osmdata::available_tags(key)`.
#' @param geometry geometry to output "polygons", "points", "lines",
#'   "multilines", or "multipolygons"; if NULL return a sf object with mixed
#'   geometry (if multiple geometry types are present). Default NULL
#' @param crs coordinate reference system for output data; if NULL, the data
#'   remains in the OSM CRS 4326. Default: NULL.
#' @param osmdata If TRUE return a osmdata class object that includes the
#'   overpass API call, metadata including timestamp and version numbers, and
#'   all available geometry types; defaults to FALSE.
#' @inheritParams st_bbox_adj
#' @export
#' @importFrom sf st_as_sfc st_as_sf st_transform st_crop st_intersection
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' @importFrom purrr pluck
get_osm_data <- function(location = NULL,
                         key,
                         value = NULL,
                         dist = NULL,
                         diag_ratio = NULL,
                         asp = NULL,
                         crs = NULL,
                         geometry = NULL,
                         osmdata = FALSE) {
  if (!is.null(geometry)) {
    osm_geometry <-
      match.arg(geometry,
        c(
          "polygons",
          "points",
          "lines",
          "multilines",
          "multipolygons"
        ),
        several.ok = TRUE
      )
  }

  osm_crs <- 4326

  # Get adjusted bounding box if any adjustment variables provided
  bbox_osm <- st_bbox_adj(
    x = location,
    dist = dist,
    diag_ratio = diag_ratio,
    asp = asp,
    crs = osm_crs
  )

 if (key == "building" && is.null(value)) {
   value <- "all"
 }

  if (value == "all") {
    value <- osmdata::available_tags(key)
  }

  query <- try(osmdata::opq(bbox = bbox_osm, timeout = 90), silent = TRUE)
  data <-
    osmdata::osmdata_sf(
      osmdata::add_osm_feature(query, key = key, value = value)
    )

  if (!osmdata) {
    data <- osmdata::unique_osmdata(data)
    data <- purrr::map_dfr(
      osm_geometry,
      ~ data[[paste0("osm_", .x)]]
    )

    if (!is.null(crs)) {
      data <- sf::st_transform(data, crs)
    }
  } else {
    data
  }

  if (getOption("osm_data_attribution", TRUE)) {
    # transition message for 0.4-0 to 0.5-0
    usethis::ui_info("Attribution is required when you use Open Street Map data.
                     See {usethis::ui_value('https://www.openstreetmap.org/copyright')} for more information on the Open Database Licence.")
    options("osm_data_attribution" = FALSE)
  }

  return(data)
}
