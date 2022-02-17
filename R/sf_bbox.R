#' Convert a bounding box into simple feature object
#'
#' Take a bbox and return an sf object.
#'
#' @param bbox bbox object
#' @return sf object
#' @seealso
#'  - \code{\link[sf]{st_as_sf}},\code{\link[sf]{st_as_sfc}}
#'  - \code{\link[sfx]{st_extent}}
#' @rdname sf_bbox_to_sf
#' @export
#' @importFrom sf st_as_sf st_as_sfc
sf_bbox_to_sf <- function(bbox) {
  sf::st_as_sf(sf::st_as_sfc(bbox))
}

#' Convert a bounding box to well known text (wkt)
#'
#' Take a bbox and return well known text for use in the wkt_filter parameter of
#' \code{\link[sf]{read_sf}}.
#'
#' @param bbox bbox object
#' @return well known text
#' @seealso
#'  \code{\link[sf]{st_as_text}},\code{\link[sf]{st_as_sfc}}
#' @rdname sf_bbox_to_wkt
#' @export
#' @importFrom sf st_as_text st_as_sfc
sf_bbox_to_wkt <- function(bbox) {
  # Convert bbox to well known text
  sf::st_as_text(sf::st_as_sfc(bbox))
}


#' Convert a bounding box to lon/lat query string
#'
#' Take a bounding box and return a SQL style query string with lon/lat values.
#'
#' @param bbox bounding box
#' @param coords query column with lon/lat coordinates
#' @param crs coordinate reference system to use for query; default 4326
#' @param lonlat Default TRUE. If FALSE, coords is assumed to be lat/lon order instead of lon/lat.
#' @importFrom glue glue
#' @importFrom sf st_transform st_bbox
#' @export
sf_bbox_to_lonlat_query <- function(bbox, coords = c("longitude",  "latitude"), crs = 4326, lonlat = TRUE) {

  if(sf::st_crs(bbox)$input != crs) {
    bbox_sf <- sf::st_transform(sf_bbox_to_sf(bbox), crs)
    bbox <- sf::st_bbox(bbox_sf)
  }

  if (!lonlat) {
    coords <- rev(coords)
  }

  glue::glue("({coords[1]} >= {bbox$xmin[[1]]}) AND ({coords[1]} <= {bbox$xmax[[1]]}) AND {coords[2]} >= {bbox$ymin[[1]]}) AND ({coords[2]} <= {bbox$ymax[[1]]})")
}

sf_bbox_asp <- function(bbox) {
  xdist <- sf_bbox_xdist(bbox) # Get width
  ydist <- sf_bbox_ydist(bbox) # Get height
  bbox_asp <- as.numeric(xdist / ydist) # Get width to height aspect ratio for bbox
  return(bbox_asp)
}


sf_bbox_xdist <- function(bbox) {
  abs(bbox["xmax"] - bbox["xmin"]) # Get width
}

sf_bbox_ydist <- function(bbox) {
  abs(bbox["ymax"] - bbox["ymin"]) # Get height
}

