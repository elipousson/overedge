#' @title Scale or rotate a simple feature object
#' @description Scale or rotate a simple feature object using affine transformations
#' @param x object of class sf
#' @param scale numeric; scale factor, Default: 1
#' @param rotate numeric; degrees to rotate (-360 to 360), Default: 0
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(ggplot2)
#'  library(overedge)
#'
#'  nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#'  nc_rotated <- st_scale_rotate(nc, scale = 0.5, rotate = 15)
#'
#'  ggplot() +
#'   geom_sf(data = nc) +
#'   geom_sf(data = nc_rotated, fill = NA, color = "red")
#'  }
#' }
#' @seealso
#' \code{\link[sf]{geos_unary}}
#' @rdname st_scale_rotate
#' @export
#' @importFrom sf st_crs st_geometry st_centroid
#' @md
st_scale_rotate <- function(x, scale = 1, rotate = 0) {
  # rotate function (see here: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  crs <- sf::st_crs(x)
  geom <- sf::st_geometry(x)
  center <- sf::st_centroid(geom)

  y <- (geom - center) * rot(pi / (360 / (rotate * 2)))
  y <- y * scale + center

  sf::st_geometry(x) <- y
  sf::st_crs(x) <- crs
  x
}
