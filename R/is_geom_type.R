#' What geometry type is this feature?
#'
#' A flexible wrapper for [sf::st_geometry_type].
#'
#' @name is_geom_type
#' @param x A sf or sfc object passed to [sf::st_geometry_type]
#' @param ext For st_geom_type, if ext TRUE and check is NULL, return a list with checks for POINTS,
#'   POLYGONS, LINESTRING, and the returned types.
#' @param type If "POINT", check if geometry type is POINT. Same for all
#'   available geometry types; not case sensitive; Default: NULL
#' @param by_geometry Passed to sf::st_geometry_type; defaults to FALSE
#' @returns Returns vector with all geometry types; gives warning if object uses
#'   multiple types.
#' @export
#' @importFrom sf st_geometry_type
is_geom_type <- function(x, ext = TRUE, type = NULL, by_geometry = FALSE) {
  geom_type <- sf::st_geometry_type(x = x, by_geometry = by_geometry)

  if (!is.null(type) && ext) {
    geom_type <- (toupper(type) %in% geom_type)
  } else if (ext) {
    geom_type <-
      list(
        "TYPES" = geom_type,
        "POINTS" = grepl("POINT$", geom_type),
        "POLYGONS" = grepl("POLYGON$", geom_type),
        "LINESTRINGS" = grepl("STRING$", geom_type),
        "COLLECTION" = geom_type %in% "GEOMETRYCOLLECTION",
        "OTHER" = geom_type %in% c("GEOMETRY", "CIRCULARSTRING", "COMPOUNDCURVE", "CURVEPOLYGON", "MULTICURVE", "MULTISURFACE", "CURVE", "SURFACE", "POLYHEDRALSURFACE", "TIN", "TRIANGLE")
      )
  }

  return(geom_type)
}

#' @name is_point
#' @rdname is_geom_type
#' @export
is_point <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    ext = TRUE,
    type = "POINT",
    by_geometry = by_geometry
  )
}

#' @name is_multipoint
#' @rdname is_geom_type
#' @export
is_multipoint <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    ext = TRUE,
    type = "MULTIPOINT",
    by_geometry = by_geometry
  )
}

#' @name is_string
#' @rdname is_geom_type
#' @export
is_string <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    ext = TRUE,
    type = "STRING",
    by_geometry = by_geometry
  )
}

#' @name is_multistring
#' @rdname is_geom_type
#' @export
is_multistring <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    ext = TRUE,
    type = "MULTISTRING",
    by_geometry = by_geometry
  )
}

#' @name is_polygon
#' @rdname is_geom_type
#' @export
is_polygon <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    ext = TRUE,
    type = "POLYGON",
    by_geometry = by_geometry
  )
}


#' @name is_multipolygon
#' @rdname is_geom_type
#' @export
is_multipolygon <- function(x, by_geometry = FALSE) {
  is_geom_type(
    x = x,
    ext = TRUE,
    type = "MULTIPOLYGON",
    by_geometry = by_geometry
  )
}
