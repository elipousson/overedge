#' Get bearing from simple feature objects with LINESTRING geometry
#'
#' @param x A `sf` object.
#' @param dir Logical indicator whether to include direction in bearing; If
#'   `FALSE`, return the absolute (positive) bearing value. If `TRUE`, return
#'   negative and positive bearing values. Default: `FALSE`.
#' @param crs Coordinate reference system passed to [sf::st_coordinates()] (must
#'   be geographic not projected). Default: 4326.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(ggplot2)
#'
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'   nc_pts <- sf::st_centroid(nc)
#'   nc_line <-
#'     sf::st_as_sf(
#'       sf::st_cast(
#'         sf::st_union(c(nc_pts[1, ]$geometry, nc_pts[30, ]$geometry)),
#'         "LINESTRING"
#'       )
#'     )
#'
#'   ggplot() +
#'     geom_sf(data = nc) +
#'     geom_sf(data = nc_pts) +
#'     geom_sf(data = st_bearing(nc_line), aes(color = bearing))
#' }
#' }
#' @seealso
#'  \code{\link[lwgeom]{st_startpoint}},\code{\link[lwgeom]{st_endpoint}}
#'  \code{\link[geosphere]{bearing}}
#' @rdname st_bearing
#' @export
#' @importFrom dplyr bind_cols
st_bearing <- function(x, dir = FALSE, crs = 4326) {
  pts <- get_start_end_point(x)

  is_pkg_installed("geosphere")

  x_bearing <-
    vapply(
      seq_len(length(pts$start$lon)),
      function(x) {
        geosphere::bearing(p1 = pts$start[x, ], p2 = pts$end[x, ])
      }, 1.0
    )

  if (!dir) {
    x_bearing <- abs(x_bearing)
  }

  dplyr::bind_cols(x, bearing = x_bearing)
}

#' Get a list of start and end points for an object
#'
#' @noRd
get_start_end_point <- function(x, crs = 4326, class = "df") {
  is_pkg_installed("lwgeom")

  pts <-
    list(
      start = as_sf(lwgeom::st_startpoint(x)),
      end = as_sf(lwgeom::st_endpoint(x))
    )

  if (!is.null(class) && class == "df") {
    pts$start <- sf_to_df(pts$start, crs = crs)
    pts$end <- sf_to_df(pts$end, crs = crs)
  }

  return(pts)
}

#' Create a LINESTRING based on start and end points
#'
#' @noRd
get_start_end_line <- function(x) {
  pts <- get_start_end_point(x, class = NULL)

  pts_combined <-
    purrr::map2(
      pts$start,
      pts$end,
      ~ sf::st_combine(
        c(.x, .y)
      ) %>% sf::st_cast("LINESTRING")
    ) # %>%
  #   dplyr::bind_rows() %>%
  #  as_sf()

  return(pts_combined)
}
