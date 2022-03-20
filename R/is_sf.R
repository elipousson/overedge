#' @noRd
is_class <- function(x, check = NULL) {
  any(check %in% class(x))
}

#' Check the class and attributes of a simple feature object
#'
#' @param x An `sf`, `sfc`, or `bbox` object.
#' @param y An sf object or a character or numeric object supported by
#'   [sf::st_crs] that can be compared to x. (used by [is_same_crs])
#' @param ext If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or
#'   not; defaults to `FALSE`. (used by [is_sf])
#' @details
#' - [is_sf]: check if x it a `sf` class object.
#' - [is_bbox]: check if x is a `bbox` class object.
#' - [is_sf_list]: check if x is a named list of `sf` class objects.
#' @export
#' @md
is_sf <- function(x, ext = FALSE) {
  if (!ext) {
    is_class(x, check = "sf")
  } else {
    is_class(x, check = c("sf", "sfc", "bbox"))
  }
}

#' @name is_sfc
#' @rdname is_sf
#' @export
is_sfc <- function(x) {
  is_class(x, check = "sfc")
}

#' @name is_bbox
#' @rdname is_sf
#' @export
is_bbox <- function(x) {
  is_class(x, "bbox")
}

#' @rdname is_sf
#' @name is_sf_list
#' @param is_named If TRUE, check if sf list is named; defaults FALSE
#' @export
is_sf_list <- function(x, is_named = FALSE, ext = FALSE) {
  is_sf_list <- is.list(x) && all(vapply(x, function(x) {
    is_sf(x, ext = ext)
  }, TRUE))
  if (is_named) {
    is_named <- !is.null(names(x)) && !("" %in% names(x))
    is_sf_list && is_named
  } else {
    is_sf_list
  }
}

#' @name is_same_crs
#' @rdname  is_sf
#' @importFrom sf st_crs
#' @export
is_same_crs <- function(x, y) {
  return(sf::st_crs(x) == sf::st_crs(y))
}

#' @name is_raster
#' @rdname is_sf
#' @export
is_raster <- function(x) {
  is_class(x, "RasterLayer")
}

#' @name is_sp
#' @rdname is_sf
#' @export
is_sp <- function(x) {
  any(grepl("Spatial", class(x)))
}
