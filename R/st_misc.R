#' Modify the geometry of a simple feature or bounding box object
#'
#' Support both bbox and sf objects as inputs.
#'
#'  - Scale or rotate a simple feature or bounding box object using affine transformations
#'  - Get the center point of a simple feature or bounding box object
#'  - Get an approximate inscribed square within a simple feature or bounding box object
#'
#' st_inscribed_square wraps `sf::st_inscribed_circle()` but limits the
#'   circle to 1 segment per quadrant (`nQuadSegs` = 1) and then rotates the resulting geometry 45
#'   degrees to provide a (mostly) inscribed square. A different rotation value
#'   can be provided to change the orientation of the shape, e.g. rotate = -45 to
#'   return a diamond shape.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(ggplot2)
#'
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'   nc_rotated <- st_scale_rotate(nc, scale = 0.5, rotate = 15)
#'
#'   ggplot() +
#'     geom_sf(data = nc) +
#'     geom_sf(data = nc_rotated, fill = NA, color = "red")
#' }
#' }
#' @param x A sf or bbox object
#' @param scale numeric; scale factor, Default: 1
#' @param rotate numeric; degrees to rotate (-360 to 360), Default: 0
#' @seealso
#' \code{\link[sf]{geos_unary}}
#' @name st_misc
#' @md
NULL

#' @rdname st_misc
#' @name st_scale_rotate
#' @export
#' @importFrom sf st_geometry st_crs
st_scale_rotate <- function(x, scale = 1, rotate = 0) {
  x <- check_to_sf(x)

  # rotate function (see here: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  center <- st_center(x, ext = TRUE)

  y <- (center$geom - center$sfc) * rot(pi / (360 / (rotate * 2)))
  y <- y * scale + center$sfc

  sf::st_geometry(x) <- y
  sf::st_crs(x) <- center$crs
  x
}

#' @rdname st_misc
#' @name st_center
#' @param ext If `TRUE`, st_center returns a list of sf, sfc, crs, and geom
#'   objects; defaults TRUE. If `FALSE`, return an sf object.
#' @param ... Additional parameters passed to `sf::st_centroid()` by st_center
#' @export
#' @importFrom sf st_crs st_geometry st_centroid st_sf
st_center <- function(x,
                      ext = TRUE,
                      ...) {
  x <- check_to_sf(x)

  if (ext) {
    crs <- sf::st_crs(x)
    geom <- sf::st_geometry(x)
    centroid <- suppressWarnings(sf::st_centroid(geom, ...))

    return(
      list(
        "sf" = sf::st_sf(centroid),
        "sfc" = centroid,
        "crs" = crs,
        "geom" = geom
      )
    )
  } else {
    suppressWarnings(sf::st_centroid(x, ...))
  }
}

#' @rdname st_misc
#' @name st_inscribed_square
#' @export
#' @importFrom sf st_is_longlat st_inscribed_circle st_geometry st_dimension st_set_geometry
#' @importFrom usethis ui_stop
#' @importFrom purrr discard
st_inscribed_square <- function(x, rotate = 0) {
  x <- check_to_sf(x)

  if (sf::st_is_longlat(x)) {
    usethis::ui_stop("st_inscribed_square does not work for data using geographic coordinates.
                      Use sf::st_transform() to change the data to use projected coordinates to use this function.")
  }

  geom <- sf::st_inscribed_circle(sf::st_geometry(x), nQuadSegs = 1)
  geom <- purrr::discard(geom, ~ is.na(sf::st_dimension(.x)))
  x <- sf::st_set_geometry(x, geom)
  x <- st_scale_rotate(x, rotate = (rotate + 45))
}


#' @rdname st_misc
#' @name st_geom_type
#' @param ext For st_geom_type, if ext TRUE and check is NULL, return a list with checks for POINTS,
#'   POLYGONS, LINESTRING, and the returned types.
#' @param check If "POINT", check if geometry type is POINT. Same for all
#'   available geometry types; Default: NULL
#' @param by_geometry Passed to sf::st_geometry_type; defaults to FALSE
#' @returns Returns vector with all geometry types; gives warning if object uses
#'   multiple types.
#' @export
#' @importFrom sf st_geometry_type
#' @importFrom usethis ui_warn
st_geom_type <- function(x, ext = TRUE, check = NULL, by_geometry = FALSE) {
  geom_type <- sf::st_geometry_type(x, by_geometry = by_geometry)

  if (!is.null(check)) {
    geom_type <- unique(geom_type)

    return(all(check %in% geom_type))
  } else if (ext) {
    check_type <-
      list(
        POINTS = grepl("POINT$", geom_type),
        POLYGONS = grepl("POLYGON$", geom_type),
        LINESTRINGS = grepl("STRING$", geom_type),
        # FIXME: This is only a partial set of geometry types
        TYPES = geom_type
        )

    return(check_type)
  } else {
    if (length(geom_type) > 1) {
      usethis::ui_warn("The sf object provded contains multiple geometry types: {geom_type}")
    }

    geom_type
  }
}
