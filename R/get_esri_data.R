#' Use esri2sf to get data from an ArcGIS FeatureServer or MapServer for a
#' location
#'
#' Wraps the \code{\link[esri2sf]{esri2sf}} and \code{\link[esri2sf]{esri2df}}
#' function to download an ArcGIS FeatureServer or MapServer. Supports spatial
#' filtering with bounding box based on location and filtering by location name
#' (if location name column is provided).
#'
#' @param location sf, sfc, or bbox object (or other object convertible with
#'   [as_bbox()]. Optional.
#' @param url FeatureServer or MapServer url to retrieve data from. Passed to
#'   \code{url} parameter of [esri2sf::esri2sf()] or
#'   \code{\link[esri2sf]{esri2df}} functions.
#' @param where string for where condition. Default is 1=1 for all rows.
#' @param where where query string passed to esri2sf, Default: NULL
#' @param coords_col coordinate columns, e.g. c("longitude", "latitude")
#' @param name_col name of ArcGIS FeatureServer or MapServer column with
#'   location names for features
#' @param name location name
#' @param ... Additional arguments passed to [esri2sf::esri2df] or
#'   [esri2sf::esri2sf]
#' @inheritParams sf_bbox_to_lonlat_query
#' @inheritParams st_bbox_ext
#' @seealso
#'  \code{\link[esri2sf]{esri2sf}}
#' @rdname get_esri_data
#' @export
#' @importFrom glue glue
#' @importFrom janitor clean_names
get_esri_data <- function(location = NULL,
                          url,
                          dist = NULL,
                          diag_ratio = NULL,
                          unit = NULL,
                          asp = NULL,
                          crs = NULL,
                          where = NULL,
                          coords_col = NULL,
                          name_col = NULL,
                          name = NULL,
                          ...) {
  is_pkg_installed(pkg = "esri2sf", repo = "yonghah/esri2sf")

  if (!is.null(location)) {
    # Adjust bounding box
    bbox <- st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp
    )

    if (!is.null(coords_col)) {
      # Get Table (no geometry) by filtering coordinate columns with bbox
      where <- c(
        where,
        sf_bbox_to_lonlat_query(bbox = bbox, coords = coords_col)
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
        where = where,
        bbox = bbox,
        crs = crs,
        progress = TRUE,
        ...
      )
    }
  } else if (!is.null(name_col)) {
    where <- c(
      where,
      glue::glue("{name_col} = '{name}'")
    )

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
      ...
    )
  }

  if (!is.null(coords_col)) {
    # Convert Table to sf object if coordinate columns exist
    data <- df_to_sf(data, coords = coord_col)
  }

  data <- janitor::clean_names(data, "snake")

  return(data)
}
