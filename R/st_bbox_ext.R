#' Get bounding box buffered to aspect ratio
#'
#' Takes an area as an sf object or a bounding box and returns a bounding box
#' that matches the provided aspect ratio and contains the area or bounding box
#' provided.
#'
#' Common aspect ratios include "1:1" (1), "4:6" (0.666), "8.5:11", "16:9"
#' (1.777). The asp parameter supports both numeric values and character
#' strings with ratios matching the format of "width:height".
#'
#'
#' @param x \code{sf} to adjust
#' @inheritParams st_buffer_ext
#' @inheritParams get_asp
#' @param crs Coordinate reference system of bounding box to return
#' @param sf If `TRUE`, return a `sf` object; defaults to `FALSE`.
#' @return Class \code{bbox} object
#' @aliases st_bbox_adj
#' @name st_bbox_ext
#' @export
#' @importFrom sf st_sf st_as_sfc st_bbox st_as_sf st_transform
st_bbox_ext <- function(x = NULL,
                        dist = NULL,
                        diag_ratio = NULL,
                        asp = NULL,
                        unit = NULL,
                        crs = NULL,
                        sf = FALSE) {

  # Convert objects to sf if needed
  if (check_bbox(x)) {
    x <- sf_bbox_to_sf(x)
  } else if (check_sfc(x)) {
    x <- sf::st_sf(x)
  } else if (check_raster(x)) {
    x <- sf::st_as_sfc(sf::st_bbox(x))
  } else if (check_spatial(x)) {
    x <- sf::st_as_sf(x)
  }

  # Get buffered area
  x <-
    st_buffer_ext(
      x = x,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit
    )

  if (!is.null(crs)) {
    x <- sf::st_transform(x, crs)
  }

  if (!is.null(asp)) {
    # Get aspect adjusted bbox
    bbox <-
      st_bbox_asp(
        x = x,
        asp = asp
      )
  } else {
    bbox <-
      sf::st_bbox(x)
  }

  if (sf) {
    return(sf_bbox_to_sf(bbox))
  } else {
    return(bbox)
  }
}

#' @rdname st_bbox_ext
#' @name st_bbox_asp
#' @export
#' @importFrom sf st_geometry_type st_bbox
#' @importFrom checkmate test_class
st_bbox_asp <- function(x = NULL,
                        asp = NULL) {
  if (check_bbox(x)) {
    bbox <- x
  } else if (st_geom_type(x, check = "POINT")) {
    x <- st_buffer_ext(x, dist = 1)
    bbox <- sf::st_bbox(x)
  } else {
    bbox <- sf::st_bbox(x)
  }

  # Get adjusted aspect ratio
  asp <- get_asp(asp = asp)

  if (!is.null(asp)) {

    # Get width/height
    xdist <- sf_bbox_xdist(bbox) # Get width
    ydist <- sf_bbox_ydist(bbox) # Get height

    # Set default nudge to 0
    x_nudge <- 0
    y_nudge <- 0

    # Compare adjust aspect ratio to bbox aspect ratio
    if (asp >= sf_bbox_asp(bbox)) {
      # adjust x
      x_nudge <- (asp * ydist - xdist) / 2
    } else {
      # adjust y
      y_nudge <- ((xdist / asp) - ydist) / 2
    }

    bbox <-
      sf_bbox_shift(
        bbox = bbox,
        x_nudge = x_nudge,
        y_nudge = y_nudge,
        side = c("top", "bottom", "left", "right"),
        dir = "out"
      )
  }

  return(bbox)
}
