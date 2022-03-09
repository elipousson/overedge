#' Convert an object to a simple feature or bounding box object
#'
#' Both functions will pass a NULL value without returning an error.
#'
#' @param x A `sf`, `bbox`, `sfc`, `raster`, `sp`, or `dataframe` object that
#'   can be converted into a simple feature or bounding box object. [as_bbox()]
#'   can also convert a vector with xmin, ymin, xmax, and ymax values.
#' @param crs Coordinate reference system; used for [as_bbox()]
#' @param ... Additional parameters passed to [sf::st_bbox()] when calling
#'   [as_bbox()] or passed to [sf::st_sf()], [sf::st_as_sf()], or [df_to_sf()]
#'   for [as_sf()] (depending on class of x)
#' @importFrom sf st_sf st_as_sfc st_bbox st_as_sf
as_sf <- function(x, ...) {
  # Convert objects to sf if needed
  if (check_sf(x)) {
    return(x)
  } else {
    if (check_bbox(x)) {
      x <- sf_bbox_to_sf(x)
    } else if (check_sfc(x)) {
      x <- sf::st_sf(x, ...)
    } else if (check_raster(x)) {
      x <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(x)), ...)
    } else if (check_sp(x)) {
      x <- sf::st_as_sf(x, ...)
    } else if (is.data.frame(x)) {
      x <- df_to_sf(x, ...)
    }
    return(x)
  }
}

#' @name as_bbox
#' @rdname as_sf
#' @importFrom sf st_bbox st_as_sf
as_bbox <- function(x, crs = 4326, ...) {
  # Convert objects to sf if needed
  if (check_bbox(x)) {
    return(x)
  } else {
    if (check_sf(x, ext = TRUE)) {
      if (st_geom_type(x, check = "POINT")) {
        x <- st_buffer_ext(x, dist = 1)
      }

      x <- sf::st_bbox(x, ...)
    } else if (check_raster(x)) {
      x <- sf::st_bbox(x, ...)
    } else if (check_sp(x)) {
      x <- sf::st_bbox(sf::st_as_sf(x), ...)
    } else if (length(x) == 4) {
      x <- sf::st_bbox(c(
        xmin = x[1], ymin = x[2],
        xmax = x[3], ymax = x[4]
      ),
      crs = crs, ...
      )
    } else if (is.data.frame(x)) {
      x <- sf::st_bbox(df_to_sf(x), ...)
    }

    return(x)
  }
}
