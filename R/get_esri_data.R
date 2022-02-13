#' Get data from an ArcGIS FeatureServer or MapServer
#'
#' Wraps the \code{esri2sf::esri2sf()} function to download an ArcGIS
#' FeatureServer or MapServer.
#'
#' @param location \code{sf} object. Optional. Only used if trim is TRUE.
#' @param url FeatureServer or MapServer url to retrieve data from. Passed to
#'   \code{url} parameter of \code{esri2sf::esri2sf()} function.
#' @param where string for where condition. Default is 1=1 for all rows.
#' @param trim Logical. Default \code{FALSE}. If \code{TRUE}, area is required.
#' @param where PARAM_DESCRIPTION, Default: '1=1'
#' @inheritParams sf_bbox_to_lonlat_query
#' @inheritParams st_bbox_adj
#' @seealso
#'  \code{\link[esri2sf]{esri2sf}}
#' @rdname get_esri_data
#' @export
#' @importFrom esri2sf esri2sf esri2df
#' @importFrom sf st_intersection
#' @importFrom janitor clean_names
get_esri_data <- function(location = NULL,
                          url,
                          dist = NULL,
                          diag_ratio = NULL,
                          asp = NULL,
                          crs = NULL,
                          where = NULL,
                          coords_col = NULL,
                          lonlat = TRUE,
                          locationname_col = NULL,
                          locationname,
                          ...) {
  if (!is.null(location)) {
    # Adjust bounding box
    bbox <- st_bbox_adj(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      asp = asp
    )

    if (!is.null(coords_col)) {
      # Get Table (no geometry) by filtering coordinate columns with bbox
      where <- c(
        where,
        sf_bbox_to_lonlat_query(bbox = bbox, coords = coords_col, lonlat = lonlat)
      )
      data <- esri2sf::esri2df(
        url = url,
        where = paste(where[!is.na(where)], collapse = " AND "),
        ...
      )
    } else {
      # Get FeatureServer with geometry
      data <- esri2sf::esri2sf(
        url = url,
        where = where, bbox = bbox, crs = crs, progress = TRUE,
        ...)
    }
  } else if (!is.null(locationname_col)) {

    where <- c(where,
               glue::glue("{locationname_col} = '{locationname}'"))

    meta <- esri2sf::esrimeta(url)

    if (meta$type == "Table") {
      # Get Table (no geometry) with location name column
      data <- esri2sf::esri2df(
        url = url,
        where = paste(where[!is.na(where)], collapse = " AND "),
        ...
      )
    } else {
      data <- esri2sf::esri2sf(
        url = url,
        where = paste(where[!is.na(where)], collapse = " AND "),
        ...
      )
    }
  } else {

    if (is.null(where)) {
      where <- "1=1"
    }

    # Get FeatureServer with no bounding box
    data <- esri2sf::esri2sf(
      url = url,
      where = where,
      progress = TRUE,
      ...)
  }

  if (!is.null(coords_col)) {
    # Convert Table to sf object if coordinate columns exist
    data <- df_to_sf(data, coords = coord_col)
  }

  data <- janitor::clean_names(data, "snake")

  return(data)
}
