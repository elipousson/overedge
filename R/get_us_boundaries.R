#' Use USAboundariesData to get current or past U.S. county boundaries for a location
#'
#' Wraps {USAboundariesData} us counties data to effectively make a version of
#' the `USAboundaries::us_counties()` that works with an adjusted location as a
#' spatial filter but does not support the support the map_date parameter for
#' historical county boundary data.
#'
#' @inheritParams get_location_data
#' @param detailed If `TRUE`, download "hires" version of county boundaries. (Not
#'   currently supported).
#' @param historical If `TRUE`, return historical data (use
#'   `USAboundaries::us_counties()` if returning geometry for a specific
#'   map_date is required).
#' @return A `sf` object with county boundaries (contemporary or historical)
#' @seealso
#'  [tigris::states()]
#' @rdname get_us_counties
#' @noRd
#' @importFrom dplyr select
#' @importFrom sf st_make_valid
get_us_counties <- function(location = NULL,
                            dist = NULL,
                            diag_ratio = NULL,
                            unit = NULL,
                            asp = NULL,
                            historical = FALSE,
                            crs = NULL,
                            crop = FALSE,
                            trim = FALSE,
                            detailed = FALSE) {
  is_pkg_installed("USAboundaries")
  is_pkg_installed("USAboundariesData")

  package <- "USAboundariesData"
  data <- "counties_contemporary_lores"

  if (historical) {
    data <- "counties_historical_lores"
  }

  counties <-
    read_sf_pkg(
      data = data,
      package = package
    )

  # FIXME: This is a work around  due to a duplicate column name in the USAboundariesData package data
  # I plan to submit an issue to pull request to fix the issue so this step is not needed.
  counties <- dplyr::select(counties, -9)
  counties <- sf::st_make_valid(counties)

  location_counties <-
    get_location_data(
      data = counties,
      location = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crop = crop,
      from_crs = 4326,
      crs = crs,
      trim = trim
    )

  if (detailed) {
    location_counties <-
      read_sf_pkg(
        data = gsub("_lores", "_hires", data),
        package = package # ,
        # FIXME: add a query that can be passed to read_sf that returns a subset of the data
        #  query = ""
        # TODO: check if bbox can work with data that may be in RDS format; not shapefile, gpkg, or geojson
      )
  }

  location_counties
}
