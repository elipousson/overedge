#' Cast geometry to another type
#'
#' Wrapper for [sf::st_cast()] that currently only supports casting MULTIPOLYGON
#' to POLYGON or MULTIPOLYGON or POLYGON to POINT.
#'
#' @param x A `sf` or `sfc` object to cast to another type.
#' @inheritParams sf::st_cast
#' @seealso
#'  \code{\link[sf]{st_cast}}
#' @rdname st_cast_ext
#' @export
#' @importFrom sf st_cast
st_cast_ext <- function(x, to = "POINT", ...) {
  geom_type <- st_geom_type(x, ext = FALSE)

  if (any(geom_type %in% c("MULTIPOLYGON", "POLYGON"))) {
    repeat {
      type_to <- switch(as.character(geom_type),
        "MULTIPOLYGON" = "POLYGON",
        "POLYGON" = "POINT"
      )

      x <- sf::st_cast(x, to = type_to, warn = FALSE, ...)
      geom_type <- st_geom_type(x, ext = FALSE)

      if (geom_type == to) break
    }
  }

  return(x)
}
