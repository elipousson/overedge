#' Get bounding box buffered and adjusted to match aspect ratio
#'
#' Takes an area as an sf object or a bounding box and returns a bounding box
#' that matches the provided aspect ratio and contains the area or bounding box
#' provided.
#'
#' Common aspect ratios include "1:1" (1), "4:6" (0.666), "8.5:11", "16:9"
#' (1.777). The asp parameter supports both numeric values and character
#' strings with ratios matching the format of "width:height".
#'
#' @title Get bounding box buffered and adjusted to aspect ratio
#' @param x \code{sf} to adjust
#' @inheritParams st_buffer_ext
#' @inheritParams st_bbox_asp
#' @param crs Coordinate reference system of bounding box to return
#' @return Class \code{bbox} object
#' @export
#' @importFrom checkmate test_class
#' @importFrom sf st_transform
st_bbox_adj <- function(x = NULL,
                        dist = NULL,
                        diag_ratio = NULL,
                        asp = NULL,
                        unit = NULL,
                        crs = NULL) {
  if (checkmate::test_class(x, "bbox")) {
    x <- sf_bbox_to_sf(x)
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
    st_bbox_asp(
      x = x,
      asp = asp
    )
  } else {
    sf::st_bbox(x)
  }
}


#' Get bounding box adjusted to match aspect ratio
#'
#' Get bbox from sf or bbox object adjusted to match an aspect ratio
#'
#' Takes an area as an  \code{sf} or \code{bbox} object and returns a bounding
#' box that matches the aspect ratio provided to \code{asp} and contains the
#' area or bounding box provided. Common aspect ratios include "1:1" (1), "4:6"
#' (0.666), "8.5:11", "16:9" (1.777). The asp parameter supports both numeric
#' values and character strings with ratios matching the format of
#' "width:height".
#'
#' @param x \code{sf} or bbox object
#' @param asp Aspect ratio of width to height as a numeric value (e.g. 0.33) or
#'   character (e.g. "1:3").
#' @return \code{bbox} object
#' @export
#' @importFrom stringr str_detect str_extract
#' @importFrom sf st_as_sfc st_as_sf st_bbox
st_bbox_asp <- function(x = NULL,
                        asp = NULL) {

  if (!checkmate::test_class(x, "bbox")) {
    bbox <- st_bbox(x)
  } else {
    bbox <- x
  }

  xdist <- bbox["xmax"] - bbox["xmin"] # Get width
  ydist <- bbox["ymax"] - bbox["ymin"] # Get height
  x_asp <- as.numeric(xdist) / as.numeric(ydist) # Get width to height aspect ratio for bbox

  asp <- get_asp(asp)

  if (!is.null(asp)) {
    # Compare aspect ratio to bbox aspect ratio
    if (asp >= x_asp) {
      # adjust x
      x_nudge <- (asp * ydist - xdist) / 2
      y_nudge <- 0
    } else {
      # adjust y
      y_nudge <- ((1 / asp) * xdist - ydist) / 2
      x_nudge <- 0
    }

    # Adjust bbox
    bbox[["xmin"]] <- bbox[["xmin"]] - x_nudge
    bbox[["xmax"]] <- bbox[["xmax"]] + x_nudge
    bbox[["ymin"]] <- bbox[["ymin"]] - y_nudge
    bbox[["ymax"]] <- bbox[["ymax"]] + y_nudge
  }

  return(bbox)
}
