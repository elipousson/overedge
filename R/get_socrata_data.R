#' Use RSocrata to get data from a Socrata open data portal for a location
#'
#' Get a selected dataset using Socrata Query Language (SoQL) parameters as a
#' tibble or sf object. Details on SoQL queries are found in the Socrata API
#' documentation <https://dev.socrata.com/docs/queries/>
#'
#' @param site_url A Socrata site url.
#' @param resource A Socrata dataset identifier. If resource is set to "list"
#'   and a valid site_url is provided, the function returns a list of all
#'   available resources.
#' @inheritParams st_bbox_ext
#' @param locationname_col name of column in Socrata resource with
#'   location names (e.g. County)
#' @param locationname location name to return
#' @param select SODA $select parameter. Set of columns to be returned, similar
#'   to a SELECT in SQL. <https://dev.socrata.com/docs/queries/select.html>
#' @param where SODA $where parameter. Filters the rows to be returned, similar
#'   to WHERE. <https://dev.socrata.com/docs/queries/where.html>
#' @param query SODA $query parameter. A full SoQL query string, all as one
#'   parameter. <https://dev.socrata.com/docs/queries/query.html>
#' @param geometry If `TRUE` and latitude/longitude columns available, return a
#'   \code{\link{sf}} object. Default `FALSE`.
#' @param coords Name(s) of column with coordinate data, Default: c("longitude",
#'   "latitude")
#' @param trim If area is provided, trim data to the area boundary rather than
#'   the bounding box, Default: FALSE. area must be provided if TRUE.
#' @param crs Coordinate reference system to return.
#' @examples
#' \dontrun{
#' ## Get Q2 2020 vehicle crash data for Cecil County, Maryland
#' get_socrata_data(
#'   site_url = "https://opendata.maryland.gov",
#'   resource = "65du-s3qu",
#'   where = "(year = '2020') AND (quarter = 'Q2')",
#'   locationname_col = "county_desc",
#'   locationname = "Cecil"
#' )
#' }
#' @export
#' @importFrom usethis ui_stop
#' @importFrom RSocrata ls.socrata read.socrata
#' @importFrom glue glue
#' @importFrom janitor clean_names
get_socrata_data <- function(resource = NULL,
                             select = NULL,
                             where = NULL,
                             query = NULL,
                             location = NULL,
                             dist = NULL,
                             diag_ratio = NULL,
                             unit = NULL,
                             asp = NULL,
                             locationname_col = NULL,
                             locationname = NULL,
                             coords = c("longitude", "latitude"),
                             geometry = FALSE,
                             site_url = NULL,
                             key = NULL,
                             from_crs = 4326,
                             crs = NULL) {

  # Check for an API key
  if ((is.null(key) | key == "") && !(resource == "list")) {
    usethis::ui_stop("An API key is required.")
  } else if (resource == "list") {
    url <- site_url
    resource_list <- RSocrata::ls.socrata(url = url)
    return(resource_list)
  }

  if (!is.null(location)) {
    # FIXME: Check on how to access the point or polygon data types via SODA
    # See <https://dev.socrata.com/docs/datatypes/point.html> for more information
    # Get adjusted bounding box if any adjustment variables provided
    bbox <- st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = from_crs
    )
  } else {
    bbox <- NULL
  }

  # Make parameter calls
  if (!is.null(select)) {
    select <- paste0("$select=", select)
  }

  if (!is.null(bbox)) {
    where <- paste0("$where=", paste0(c(where, sf_bbox_to_lonlat_query(bbox = bbox, coords = coords)), collapse = " AND "))
  } else if (!is.null(locationname_col) && !is.null(locationname)) {
    where <- paste0("$where=", paste0(c(where, glue::glue("{locationname_col} like '{locationname}'")), collapse = " AND "))
  } else if (!is.null(where)) {
    where <- paste0("$where=", where)
  }

  if (!is.null(query)) {
    query <- paste0("$query=", query)
  }

  if (check_url(site_url)) {
    if (grepl("/dataset/", site_url) && is.null(resource)) {
      url <- site_url
    } else if (!grepl("/dataset/", site_url)) {
      # Assemble url from resource identifier, and select, where, and query parameters
      site_url <- gsub("/$", "", site_url)
      url <- paste0(site_url, "/resource/", resource, ".json")
      if (!is.null(select) | !is.null(where) | !is.null(query)) {
        url <- paste0(url, "?", paste0(c(select, where, query), collapse = "&"))
      }
    }
  } else {
    usethis::ui_stop("A valid site_url is required.")
  }

  # Download data from Socrata Open Data portal
  resource <-
    as.data.frame(RSocrata::read.socrata(url = url, app_token = key))

  resource <-
    janitor::clean_names(resource, "snake")

  if (geometry) {
    resource <- df_to_sf(x = resource, coords = coords, crs = crs)
  }

  return(resource)
}
