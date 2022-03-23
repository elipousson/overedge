#' Transform or convert coordinates of a simple feature or bounding box object
#'
#' Take a `sf`, `sfc`, or `bbox` object and transform to coordinate reference
#' system to match the object provided to `crs`.
#'
#' @param x An `sf`, `sfc`, or `bbox` object, a list of sf objects.
#' @param crs A character or numeric reference to a coordinate reference system
#'   supported by [sf::st_crs()] or another  `sf`, `sfc`, or `bbox` object that
#'   is used to provide crs.
#' @param class Class of object to return (`sf` or `bbox`). If x is an sf list,
#'   the returned object remains a list but may be converted to `bbox` if class
#'   = "sf".
#' @return An `sf`, `sfc`, or `bbox` object transformed to a new coordinate
#'   reference system.
#' @seealso \code{\link[sf]{st_transform}},\code{\link[sf]{st_crs}}
#' @rdname st_transform_ext
#' @export
#' @importFrom sf st_transform st_crs
st_transform_ext <- function(x = NULL,
                             crs = NULL,
                             class = NULL) {
  if (!is_bbox(x) && !is_sfc(x) && is_sf_list(x, ext = TRUE)) {
    x <-
      purrr::map(
        x,
        ~ st_transform_ext(
          x = .x,
          crs = crs,
          class = class
        )
      )

    return(x)
  }

  stopifnot(
    is_sf(x, ext = TRUE)
  )

  x_is_sf <- is_sf(x)
  x_is_bbox <- is_bbox(x)

  if (!is.null(crs)) {
    is_x_same_crs <- is_same_crs(x = x, y = crs)

    if (is_sf(crs, ext = TRUE) && !is_x_same_crs) {
      # if x has a different crs than the sf object passed to crs
      crs <- sf::st_crs(x = crs)
    }

    if (x_is_bbox) {
      # If x is a bbox
      x <- sf_bbox_transform(bbox = x, crs = crs)
    } else if (!is_x_same_crs) {
      # If x is an sf or sfc object
      x <- sf::st_transform(x = x, crs = crs)
    }
  }

  if (!is.null(class)) {
    if (("bbox" %in% class) && !x_is_bbox) {
      x <- sf::st_bbox(x)
    } else if (("sf" %in% class) && !x_is_sf) {
      if (is_bbox(x)) {
        x <- sf_bbox_to_sf(x)
      } else if (("sfc" %in% class) && is_sfc(x)) {
        x <- as_sf(x)
      }
    }
  }

  return(x)
}
