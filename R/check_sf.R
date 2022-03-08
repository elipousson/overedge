#' @noRd
check_class <- function(x, check = NULL) {
  any(check %in% class(x))
}

#' Check the class and attributes of a simple feature object
#'
#' @param x An `sf`, `sfc`, or `bbox` object.
#' @param y An sf object or a character or numeric object supported by
#'   \code{\link[sf]{st_crs}} that can be compared to x. (used by
#'   check_sf_same_crs)
#' @param ext If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or
#'   not; defaults to `FALSE`. (used by check_sf)
#' @param ... Additional parameters passed to sf::st_bbox when calling check_to_bbox or
#'   passed to sf::st_sf, sf::st_as_sf, or df_to_sf for check_to_sf (depending on class of x)
#' @details
#' - check_sf: check if x it a `sf` class object.
#' - check_bbox: check if x is a `bbox` class object.
#' - check_sf_list: check if x is a named list of `sf` class objects.
#' @export
#' @md
check_sf <- function(x, ext = FALSE) {
  if (!ext) {
    check_class(x, check = "sf")
  } else {
    check_class(x, check = c("sf", "sfc", "bbox"))
  }
}

#' @name check_sfc
#' @rdname check_sf
check_sfc <- function(x) {
  check_class(x, check = "sfc")
}

#' @name check_bbox
#' @rdname check_sf
check_bbox <- function(x) {
  check_class(x, "bbox")
}

#' @rdname check_sf
#' @name check_sf_list
check_sf_list <- function(x) {
  is_sf_list <- is.list(x) && all(vapply(x, check_sf, TRUE))
  is_named <- !is.null(names(x)) && !("" %in% names(x))
  is_sf_list && is_named
}

#' @name check_to_sf
#' @rdname  check_sf
#' @importFrom sf st_sf st_as_sfc st_bbox st_as_sf
check_to_sf <- function(x, ...) {
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
    } else if (check_spatial(x)) {
      x <- sf::st_as_sf(x, ...)
    } else if (is.data.frame(x)) {
      x <- df_to_sf(x, ...)
    }
    return(x)
  }
}

#' @name check_to_bbox
#' @rdname  check_sf
#' @importFrom sf st_crs
check_to_bbox <- function(x, crs = 4326, ...) {
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
    } else if (check_spatial(x)) {
      x <- sf::st_bbox(sf::st_as_sf(x), ...)
    } else if (length(x) == 4) {
      x <- sf::st_bbox(c(
        xmin = x[1], ymin = x[2],
        xmax = x[3], ymax = x[4]
      ),
      crs = crs, ...
      )
    }

    return(x)
  }
}


#' @name check_sf_same_crs
#' @rdname  check_sf
#' @importFrom sf st_crs
check_sf_same_crs <- function(x, y) {
  return(sf::st_crs(x) == sf::st_crs(y))
}

#' @name check_raster
#' @rdname check_sf
check_raster <- function(x) {
  check_class(x, "RasterLayer")
}

#' @name check_sp
#' @rdname check_sf
check_sp <- function(x) {
  any(grepl("Spatial", class(location)))
}
