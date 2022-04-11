#' Get distance from a simple feature object to another object or bounding box
#' corner
#'
#' @param from A sf, sfc, or bbox object.
#' @param to A sf, sfc, or bbox object or length 2 character vector. If sf or
#'   sfc, must have 1 feature or the same number of features as from (if
#'   by_element is TRUE). If to is a character vector it must represent a valid
#'   xy pair using the following options: "xmin", "ymin", "xmax", "ymax",
#'   "xmid", "ymid".
#' @param units Unit to convert distance to, Default: NULL
#' @param drop If TRUE, drop units from the distance returned; defaults to FALSE.
#' @param keep_all If FALSE, return dist but not original sf, Default: TRUE
#' @inheritParams sf::st_distance
#' @rdname st_dist
#' @export
#' @importFrom sf st_centroid st_distance
#' @importFrom dplyr bind_cols
st_dist <- function(from, to, by_element = TRUE, units = NULL, drop = FALSE, keep_all = TRUE, .id = "dist") {
  stopifnot(
    is_sf(from, ext = TRUE),
    is_sf(to, ext = TRUE) || is.character(to)
  )

  x <- from
  crs <- sf::st_crs(x)

  if (!is_point(from)) {
    from <- st_center(from, ext = FALSE)
    from <- as_sf(from, crs = crs)
  }

  if (is.character(to)) {
    to <-
      match.arg(
        to,
        c("xmin", "ymin", "xmax", "ymax", "xmid", "ymid"),
        several.ok = TRUE
      )

    to <- sf_bbox_point(as_bbox(x), point = to)
    to <- as_sf(to, crs = crs)
  } else if (!is_point(to)) {
    to <- st_center(to, ext = FALSE)
    to <- as_sf(to, crs = crs)
  }

  x_dist <-
    sf::st_distance(from, to, by_element = by_element)

  if (!is.null(units)) {
    x_dist <-
      convert_dist_units(
        x_dist,
        to = units
      )
  }

  x_dist <- tibble::tibble("{.id}" := x_dist)

  if (drop) {
    x_dist <- as.numeric(x_dist[[.id]])
  }

  if (!keep_all) {
    return(x_dist)
  }

  x <-
    dplyr::bind_cols(
      x,
      x_dist
    )

  x <-
    relocate_sf_col(x)

  return(x)
}
