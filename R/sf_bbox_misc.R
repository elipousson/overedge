#' Work with simple feature bounding boxes
#'
#' Simple bounding box functions that you can use to:
#'
#' - Measure distances (sf_bbox_xdist, sf_bbox_ydist, and sf_bbox_diagdist)
#' - Get an aspect ratio (sf_bbox_asp)
#' - Transform the coordinate reference system (sf_bbox_transform)
#' - Convert a bounding box to a SQL style query (sf_bbox_to_lonlat_query), well
#' known text (sf_bbox_to_wkt), or a simple feature object (sf_bbox_to_sf)
#'
#' The only function with additional functions is sf_bbox_to_lonlat_query. All
#' other functions only take a bbox parameter.
#'
#' @param bbox bounding box object
#' @param coords query column with lon/lat coordinates
#' @param crs coordinate reference system to use for query; default 4326
#' @param lonlat Default TRUE. If FALSE, coords is assumed to be lat/lon order instead of lon/lat.
#' @name sf_bbox_misc
#' @export
#' @md
NULL

#' @aliases sf_bbox_asp
#' @rdname sf_bbox_misc
#' @export
sf_bbox_asp <- function(bbox) {
  xdist <- sf_bbox_xdist(bbox) # Get width
  ydist <- sf_bbox_ydist(bbox) # Get height
  bbox_asp <- xdist / ydist # Get width to height aspect ratio for bbox
  return(bbox_asp)
}

#' @aliases sf_bbox_xdist
#' @rdname sf_bbox_misc
#' @export
sf_bbox_xdist <- function(bbox) {
  abs(bbox["xmax"] - bbox["xmin"]) # Get width
}

#' @aliases sf_bbox_ydist
#' @rdname sf_bbox_misc
#' @export
sf_bbox_ydist <- function(bbox) {
  abs(bbox["ymax"] - bbox["ymin"]) # Get height
}

#' @aliases sf_bbox_diagdist
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_bbox st_distance st_point
sf_bbox_diagdist <- function(bbox) {
  dist <-
    sf::st_distance(
      sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
      sf::st_point(c(bbox[["xmax"]], bbox[["ymax"]]))
    )

  return(as.numeric(dist))
}

#' @aliases sf_bbox_transform
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_crs st_transform st_bbox
sf_bbox_transform <- function(bbox, crs) {
  if(sf::st_crs(bbox)$input != crs) {
    bbox_sf <- sf::st_transform(sf_bbox_to_sf(bbox), crs)
    bbox <- sf::st_bbox(bbox_sf)
  }
  return(bbox)
}

#' @param bbox bbox object
#' @return sf object
#' @seealso
#'  - \code{\link[sf]{st_as_sf}},\code{\link[sf]{st_as_sfc}}
#'  - \code{\link[sfx]{st_extent}}
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_as_sf st_as_sfc
sf_bbox_to_sf <- function(bbox) {
  sf::st_as_sf(sf::st_as_sfc(bbox))
}

#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_as_text st_as_sfc
sf_bbox_to_wkt <- function(bbox) {
  # Convert bbox to well known text
  sf::st_as_text(sf::st_as_sfc(bbox))
}

#' @rdname sf_bbox_misc
#' @export
#' @importFrom glue glue
#' @importFrom sf st_transform st_bbox
sf_bbox_to_lonlat_query <- function(bbox, coords = c("longitude",  "latitude"), crs = 4326, lonlat = TRUE) {

  bbox <- sf_bbox_transform(bbox, crs = crs)

  if (!lonlat) {
    coords <- rev(coords)
  }

  glue::glue("({coords[1]} >= {bbox$xmin[[1]]}) AND ({coords[1]} <= {bbox$xmax[[1]]}) AND {coords[2]} >= {bbox$ymin[[1]]}) AND ({coords[2]} <= {bbox$ymax[[1]]})")
}
