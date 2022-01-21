#' @title Create an inscribed square in a simple feature object
#' @description This function wraps `sf::st_inscribed_circle()` but limits the
#'   circle to 1 segment per quadrant (`nQuadSegs` = 1) and then rotates the resulting geometry 45
#'   degrees to provide a (mostly) inscribed square. A different rotation value
#'   can be provided to change the orientation of the shap, e.g. rotate = -45 to
#'   return a diamond shape.
#' @param x object of class sf
#' @param rotate numeric; degrees to rotate square geometry, Default: 0
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(ggplot2)
#'
#'  nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#'  nc_squares <- st_inscribed_square(sf::st_transform(nc, 2264))
#'
#'  ggplot() +
#'   geom_sf(data = nc) +
#'   geom_sf(data = nc_squares, fill = NA, color = "red")

#'  }
#' }
#' @seealso
#'  \code{\link[sf]{geos_unary}}
#' @rdname st_inscribed_square
#' @export
#' @importFrom sf st_inscribed_circle st_geometry st_dimension st_set_geometry
#' @importFrom purrr discard
st_inscribed_square <- function(x, rotate = 0) {
  geom <- sf::st_inscribed_circle(sf::st_geometry(x), nQuadSegs = 1)
  geom <- purrr::discard(geom, ~ is.na(sf::st_dimension(.x)))
  x <- sf::st_set_geometry(x, geom)
  x <- st_scale_rotate(x, rotate = (rotate + 45))
}
