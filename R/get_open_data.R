#' Get data from an open data portal (Socrata) for a location
#'
#' get_socrata_data is get_open_data with source_type set to "socrata" (the only
#' currently supported option). get_open_data can return a selected dataset
#' using Socrata Query Language (SoQL) parameters as a tibble or sf object.
#' Details on SoQL queries are found in the Socrata API documentation
#' <https://dev.socrata.com/docs/queries/>.
#'
#' @param source_url A data source url. For Socrata, this should the base url
#'   for the open data portal.
#' @param source_type Data source type; defaults to "socrata" which is currently
#'   the only supported option.
#' @param data A data set identifier (known as a resource for Socrata). If data
#'   is set to "list" and a valid source_url is provided, the function returns a
#'   list of all available resources.
#' @inheritParams st_bbox_ext
#' @param locationname_col name of column in Socrata data with
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
#' get_open_data(
#'   source_url = "https://opendata.maryland.gov",
#'   data = "65du-s3qu",
#'   where = "(year = '2020') AND (quarter = 'Q2')",
#'   locationname_col = "county_desc",
#'   locationname = "Cecil",
#'   key = Sys.getenv("MARYLAND_OPEN_DATA_API_KEY")
#' )
#' }
#' @export
#' @importFrom usethis ui_stop
#' @importFrom RSocrata ls.socrata read.socrata
#' @importFrom glue glue
#' @importFrom janitor clean_names
get_open_data <- function(data = NULL,
                          source_url = NULL,
                          source_type = "socrata",
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
                          key = NULL,
                          from_crs = 4326,
                          crs = NULL) {
  if (source_type != "socrata") {
    usethis::ui_stop(
      "This function currently only supports access to Socrata open data portals.
      Other open data access options (e.g. CKAN, Flat Data) may be added in the future."
    )
  }

  # Check for an API key
  if ((is.null(key) | key == "") && !(data == "list")) {
    usethis::ui_stop("An API key is required.")
  } else if (data == "list") {
    url <- source_url
    data_list <- RSocrata::ls.socrata(url = url)
    return(data_list)
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

  if (check_url(source_url)) {
    if (grepl("/dataset/", source_url) && is.null(data)) {
      url <- source_url
    } else if (!grepl("/dataset/", source_url)) {
      # Assemble url from data identifier, and select, where, and query parameters
      source_url <- gsub("/$", "", source_url)
      url <- paste0(source_url, "/resource/", data, ".json")
      if (!is.null(select) | !is.null(where) | !is.null(query)) {
        url <- paste0(url, "?", paste0(c(select, where, query), collapse = "&"))
      }
    }
  } else {
    usethis::ui_stop("A valid source_url is required.")
  }

  # Download data from Socrata Open Data portal
  data <-
    as.data.frame(RSocrata::read.socrata(url = url, app_token = key))

  data <-
    janitor::clean_names(data, "snake")

  if (geometry) {
    data <- df_to_sf(x = data, coords = coords, crs = crs)
  }

  return(data)
}

#' @rdname get_open_data
#' @name get_socrata_data
#' @export
get_socrata_data <- function(data = NULL,
                             source_url = NULL,
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
                             key = NULL,
                             from_crs = 4326,
                             crs = NULL) {
  get_open_data(
    data = data,
    source_url = source_url,
    source_type = "socrata",
    select = select,
    where = where,
    query = query,
    location = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    locationname_col = locationname_col,
    locationname = locationname,
    coords = c("longitude", "latitude"),
    geometry = geometry,
    key = key,
    from_crs = from_crs,
    crs = crs
  )
}