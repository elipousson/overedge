#' Use osmdata to get Open Street Map data for a location
#'
#' Wraps \code{osmdata} functions to query OSM data by adjusted bounding box or
#' by enclosing ways/relations around the center of a location. timeout and
#' nodes_only parameters are not fully supported currently.
#'
#' @param location A `sf`, `sfc`, or `bbox` object.
#' @param key Feature key for overpass API query.
#' @param value Value of the feature key; can be negated with an initial
#'   exclamation mark, `value = "!this"`, and can also be a vector, `value =
#'   c("this", "that")`. If `value = "all"` or if `key = "building"` the values
#'   passed to the osmdata package are from a preset list extracted from
#'   [osmdata::available_tags()].
#' @inheritParams st_bbox_ext
#' @param geometry Geometry type to output ("polygons", "points", "lines",
#'   "multilines", or "multipolygons"); if multiple geometry types are needed
#'   set osmdata to `TRUE.` Default `NULL`.
#' @param crs Coordinate reference system for output data; if NULL, the data
#'   remains in the Open Street Map coordinate reference system 4326. Default:
#'   `NULL`.
#' @param osmdata If `TRUE` return a `osmdata` class object that includes the
#'   overpass API call, metadata including timestamp and version numbers, and
#'   all available geometry types; defaults to `FALSE`.
#' @param enclosing If enclosing is "relation" or "way", use the
#'    [osmdata::opq_enclosing()] function to query the OSM data (instead of
#'    [osmdata::add_osm_feature()]. Defaults to `NULL`. When using enclosing, the dist,
#'   diag_ratio, asp, and unit parameters are ignored and the center of the
#'   provided location is used for the query. geometry is set automatically
#'   based enclosing with "relation" using "multipolygon" and "way" using
#'   "polygon" geometry.
#' @inheritParams osmdata::opq
#' @return A simple feature object with features using selected geometry type or
#'   an `osmdata` object with features from all geometry types.
#' @export
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
                         osmdata = FALSE,
                         enclosing = NULL,
                         nodes_only = FALSE,
                         timeout = 120) {
  is_pkg_installed("osmdata")

  if ((key == "building") && is.null(value)) {
    value <- osm_building_tags
  }

  if (value == "all") {
    value <- osmdata::available_tags(key)
  }

  if (is.null(enclosing)) {
    osm_crs <- 4326

    # Get adjusted bounding box if any adjustment variables provided
    bbox_osm <-
      st_bbox_ext(
        x = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = osm_crs
      )

    bbox_osm <- sf_bbox_to_sf(bbox_osm)

    if (nodes_only) {
      query <-
        osmdata::opq(
          bbox = bbox_osm,
          nodes_only = nodes_only
        )
    } else {
      query <-
        try(
          osmdata::opq(bbox = bbox_osm),
          silent = TRUE
        )
    }

    query <-
      osmdata::add_osm_feature(
        opq = query,
        key = key,
        value = value,
        match_case = FALSE
      )
  } else if (!is.null(enclosing)) {
    enclosing <- match.arg(enclosing, c("way", "relation"))
    coords <- sf_to_df(location)

    query <-
      try(
        osmdata::opq_enclosing(
          lon = coords$lon,
          lat = coords$lat,
          key = key,
          value = value,
          enclosing = enclosing
        ),
        silent = TRUE
      )

    query <- osmdata::opq_string(opq = query)

    geometry <-
      switch(enclosing,
        "way" = "polygon",
        "relation" = "multipolygon"
      )
  }

  data <-
    suppressMessages(osmdata::osmdata_sf(query))

  osm_geometry <-
    match.arg(
      geometry,
      c(
        "polygons",
        "points",
        "lines",
        "multilines",
        "multipolygons"
      )
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
  }

  if (getOption("overedge.osm_attribution", TRUE)) {
    usethis::ui_info("Attribution is required when you use Open Street Map data.
                     See {usethis::ui_value('https://www.openstreetmap.org/copyright')} for more information on the Open Database Licence.")
    options("overedge.osm_attribution" = FALSE)
  }

  return(data)
}

#' @param level administrative level (admin_level) of boundary to return;
#'   defaults to NULL. See <https://wiki.openstreetmap.org/wiki/Key:admin_level>
#'   for more information. Only used for get_osm_boundaries.
#' @rdname get_osm_data
#' @name get_osm_boundaries
#' @export
#' @importFrom dplyr filter between
get_osm_boundaries <- function(location,
                               level = NULL,
                               enclosing = "way",
                               osmdata = TRUE) {
  boundaries <-
    get_osm_data(
      location = location,
      key = "boundary",
      value = "administrative",
      enclosing = enclosing,
      osmdata = TRUE
    )

  if (!is.null(level)) {
    boundaries <-
      dplyr::filter(
        boundaries,
        dplyr::between(admin_level, min(level), max(level))
      )
  }

  return(boundaries)
}
