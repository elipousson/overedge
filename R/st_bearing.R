#' @title Get bearing of sf object with LINESTRING geometry
#' @param x object of class sf
#' @param dir logical; If FALSE, return the absolute (positive) bearing value.
#'   If TRUE, return negative and positive bearing values. Default: FALSE.
#' @param crs Coordinate reference system passed to sf::st_coordinates() (must
#'   be geographic not projected). Default: 4326
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(ggplot2)
#'
#'  nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#'  nc_pts <- sf::st_centroid(nc)
#'  nc_line <-
#'    sf::st_as_sf(
#'      sf::st_cast(sf::st_union(c(nc_pts[1,]$geometry, nc_pts[30,]$geometry)), "LINESTRING")
#'      )
#'
#'  ggplot() +
#'    geom_sf(data = nc) +
#'    geom_sf(data = nc_pts) +
#'    geom_sf(data = st_bearing(nc_line), aes(color = bearing))
#'  }
#' }
#' @seealso
#'  \code{\link[lwgeom]{st_startpoint}},\code{\link[lwgeom]{st_endpoint}}
#'  \code{\link[geosphere]{bearing}}
#' @rdname st_bearing
#' @export
#' @importFrom sf st_transform st_coordinates
#' @importFrom lwgeom st_startpoint st_endpoint
#' @importFrom dplyr bind_cols
#' @importFrom geosphere bearing
st_bearing <- function(x, dir = FALSE, crs = 4326) {

  start <- get_coord_df(lwgeom::st_startpoint(x), crs = crs)
  end <- get_coord_df(lwgeom::st_endpoint(x), crs = crs)

  x_bearing <- geosphere::bearing(start, end)

  if (!dir) {
    x_bearing <- abs(x_bearing)
  }

  dplyr::bind_cols(x, bearing = x_bearing)
}
