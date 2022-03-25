
#' Erase an area of a simple feature object
#'
#' An extended version of [sf::st_difference] that uses the flip parameter to
#' optionally call [sf::st_intersection] and combines and unions the second
#' object by default. [st_trim] is [st_erase] with flip set to `TRUE`.
#'
#' @param x sf object
#' @param y sf object
#' @param flip If TRUE, use st_intersection; if FALSE use st_difference,
#'   Default: FALSE
#' @param union If TRUE, use st_combine and st_union on y before applying
#'   difference/intersection, Default: FALSE
#' @export
#' @importFrom sf st_union st_combine st_intersection st_difference
st_erase <- function(x, y, flip = FALSE, union = TRUE) {
  if (!is_same_crs(x, y)) {
    y <- st_transform_ext(x = y, crs = x)
  }

  if (union) {
    y <- sf::st_union(sf::st_combine(y))
  }

  if (flip) {
    x <- suppressMessages(suppressWarnings(sf::st_intersection(x, y)))
  } else {
    x <- suppressWarnings(sf::st_difference(x, y))
  }

  return(x)
}

#' @rdname st_erase
#' @name st_trim
st_trim <- function(x, y, union = TRUE) {
  st_erase(
    x = x,
    y = y,
    flip = TRUE,
    union = union
  )
}
