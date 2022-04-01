#' @noRd
is_class <- function(x, classes = NULL, null.ok = FALSE) {
  if (is.null(x) && null.ok) {
    return(TRUE)
  }

  any(classes %in% class(x))
}

#' What is the class or spatial attributes of this feature?
#'
#' @param x An `sf`, `sfc`, or `bbox` object.
#' @param y An sf object or a character or numeric object supported by
#'   [sf::st_crs] that can be compared to x. (used by [is_same_crs])
#' @param ext If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or
#'   not; defaults to `FALSE`. (used by [is_sf])
#' @param null.ok If `TRUE` and x is `NULL`, return `TRUE`; defaults to `FALSE`.
#' @param dist For [is_same_dist], bounding box dimension to compare distances for "diagdist",
#'   "xdist", or "ydist"; does not currently support numeric distances; defaults
#'   to `NULL`.
#' @param diff For [is_same_dist] and [is_same_area], If `TRUE`, return the difference
#'   between the x and y objects. If `FALSE`, return `TRUE` or `FALSE` as expected;
#'   defaults to `FALSE`.
#' @param union If `TRUE`, use [sf::st_union] on both x and y before comparing
#'   area; defaults to `FALSE`.
#' @details
#' - [is_sf]: is x a `sf` class object?
#' - [is_sfc]: is x is a `sfc` class object?
#' - [is_bbox]: is x is a `bbox` class object?
#' - [is_sf_list]: is x is a list of `sf` class objects (with or without names)?
#' - [is_raster]: is x a `Raster` class object?
#' - [is_sp]: is x a `Spatial` class object of any type?
#' - [is_same_crs]: do x and y have the same coordinate reference system?
#' - [is_same_dist]: do the bbox for x and bbox for y have the same x, y, or diagonal distance?
#' - [is_same_area]: do x and y have the same area?
#'
#' @export
#' @md
is_sf <- function(x, ext = FALSE, null.ok = FALSE) {
  if (ext) {
    classes <- c("sf", "sfc", "bbox")
  } else {
    classes <- "sf"
  }

  is_class(x, classes = classes, null.ok = null.ok)
}

#' @name is_sfc
#' @rdname is_sf
#' @export
is_sfc <- function(x, null.ok = FALSE) {
  is_class(x, classes = "sfc", null.ok = null.ok)
}

#' @name is_bbox
#' @rdname is_sf
#' @export
is_bbox <- function(x, null.ok = FALSE) {
  is_class(x, classes = "bbox", null.ok = null.ok)
}

#' @rdname is_sf
#' @name is_sf_list
#' @param is_named If `TRUE`, check if sf list is named; defaults `FALSE`.
#' @export
is_sf_list <- function(x, is_named = FALSE, ext = FALSE, null.ok = FALSE) {
  if (is.null(x) && null.ok) {
    return(TRUE)
  }

  if (is_sf(x, ext = TRUE, null.ok = FALSE)) {
    return(FALSE)
  }

  is_sf_list <- is.list(x) && all(
    vapply(
      x,
      function(x) {
        is_sf(x, ext = ext, null.ok = null.ok)
      },
      TRUE
    )
  )

  if (is_named) {
    is_named <- !is.null(names(x)) && !("" %in% names(x))
    return(is_sf_list && is_named)
  }

  return(is_sf_list)
}

#' @name is_raster
#' @rdname is_sf
#' @export
is_raster <- function(x, null.ok = FALSE) {
  is_class(x, classes = "RasterLayer", null.ok = null.ok)
}

#' @name is_sp
#' @rdname is_sf
#' @export
is_sp <- function(x, null.ok = FALSE) {
  if (is.null(x) && null.ok) {
    return(TRUE)
  }

  any(grepl("Spatial", class(x)))
}

#' @name is_same_crs
#' @rdname  is_sf
#' @importFrom sf st_crs
#' @export
is_same_crs <- function(x, y) {
  sf::st_crs(x) == sf::st_crs(y)
}

#' @name is_same_dist
#' @rdname  is_sf
#' @importFrom sf st_area
#' @export
is_same_dist <- function(x, y, dist = NULL, diff = FALSE) {
  if (is.character(dist)) {
    x <- as_bbox(x)
    y <- as_bbox(x)

    dist <- match.arg(dist, c("diagdist", "xdist", "ydist"))

    x_dist <-
      switch(dist,
        # FIXME: Is this going to work or is there a tolerance factor needed?
        "diagdist" = sf_bbox_diagdist(x),
        "xdist" = sf_bbox_xdist(x),
        "ydist" = sf_bbox_ydist(x)
      )


    y_dist <-
      switch(dist,
        # FIXME: Is this going to work or is there a tolerance factor needed?
        "diagdist" = sf_bbox_diagdist(y),
        "xdist" = sf_bbox_xdist(y),
        "ydist" = sf_bbox_ydist(y)
      )
  } else if (is.numeric(dist)) {
    # FIXME: Add support for comparing distance between X and Y to a numeric distance
    # sf::st_distance()
    # convert_dist_units(dist, to = )
  }

  if (diff) {
    return(x_dist - y_dist)
  }

  return(x_dist == y_dist)
}


#' @name is_same_area
#' @rdname  is_sf
#' @importFrom sf st_area
#' @export
is_same_area <- function(x, y, union = FALSE, diff = FALSE) {
  if (union) {
    x <- sf::st_union(x)
    y <- sf::st_union(y)
  }

  x_area <- sf::st_area(x)
  y_area <- sf::st_area(y)

  if (diff) {
    return(x_area - y_area)
  }

  return(x_area == y_area)
}
