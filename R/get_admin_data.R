#' @title Get U.S. State and County boundary data for a location
#'
#' @description Get U.S. states and counties using a very fast query.
#'
#' @param location sf, sfc, or bbox object convertible with as_sfc
#' @inheritParams st_bbox_ext
#' @name get_admin_data
#' @importFrom purrr map_lgl
#' @importFrom sf st_as_sfc st_intersects
NULL

#' @rdname get_admin_data
#' @name get_states
#' @export
get_states <- function(location,
                       dist = NULL,
                       diag_ratio = NULL,
                       unit = NULL,
                       asp = NULL) {
  if (!is.null(location)) {
    # Adjust bounding box
    bbox <- st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp
    )
  }

  intersects <-
    st_intersects_data(x = bbox, data = us_states, crs = 3857)

  return(us_states[intersects, ])
}

#' @rdname get_admin_data
#' @name get_counties
#' @export
get_counties <- function(location,
                         dist = NULL,
                         diag_ratio = NULL,
                         unit = NULL,
                         asp = NULL) {
  if (!is.null(location)) {
    # Adjust bounding box
    bbox <- st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp
    )
  }

  intersects <-
    st_intersects_data(x = bbox, data = us_counties, crs = 3857)

  return(us_counties[intersects, ])
}

#' Spatial filter with st_intersects converting wkt data
#'
#' @noRd
st_intersects_data <- function(x, data, wkt = TRUE, crs = NULL) {
  x <- as_sfc(x, crs = crs)

  if (wkt && (crs == 3857)) {
    data <- as_sfc(data$wkt)
  }

  intersects <-
    purrr::map_lgl(
      data,
      ~ sf::st_intersects(.x, x, sparse = FALSE)
    )

  return(intersects)
}
