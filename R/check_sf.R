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
