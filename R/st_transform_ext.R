#' Transform or convert coordinates of a simple feature or bounding box object
#'
#' Take a `sf`, `sfc`, or `bbox` object and transform to coordinate reference
#' system to match provided `crs` or the  CRS for the provided `y` object.
#'
#' @param x An `sf`, `sfc`, or `bbox` object, or a character or numeric object
#'   supported by \code{\link[sf]{st_crs}}
#' @param crs A coordinate reference system to convert x to or another  `sf`,
#'   `sfc`, or `bbox` object that is used to provide crs.
#' @return An `sf`, `sfc`, or `bbox` object transformed to a new coordinate
#'   reference system.
#' @seealso \code{\link[sf]{st_transform}},\code{\link[sf]{st_crs}}
#' @rdname st_transform_ext
#' @export
#' @importFrom sf st_transform st_crs
st_transform_ext <- function(x,
                             crs = NULL,
                             return = NULL) {
  stopifnot(
    check_sf(x, ext = TRUE)
  )

  if (!is.null(crs)) {
    if (check_sf(crs, ext = TRUE) && !check_sf_same_crs(x, crs)) {
      # if x has a different crs than the sf object passed to crs
      crs <- sf::st_crs(crs)
    }

    if (check_bbox(x)) {
      # If x is a bbox
      x <- sf_bbox_transform(bbox = x, crs = crs)
    } else {
      # If x is an sf or sfc object
      x <- sf::st_transform(x, crs)
      if ("bbox" %in% return) {
        x <- sf::st_bbox(x)
      }
    }
  }

  return(x)
}
