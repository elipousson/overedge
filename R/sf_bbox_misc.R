#' Measure, transform, and convert bounding boxes
#'
#' Simple bounding box functions that you can use to:
#'
#' - Measure distances ([sf_bbox_dist], [sf_bbox_xdist], [sf_bbox_ydist], and [sf_bbox_diagdist])
#' - Get an aspect ratio ([sf_bbox_asp])
#' - Transform the coordinate reference system ([sf_bbox_transform])
#' - Convert a bounding box to a SQL style query ([sf_bbox_to_lonlat_query]), well
#' known text ([sf_bbox_to_wkt]), or a simple feature object ([sf_bbox_to_sf])
#' - Shift, expand, or contract a bounding box ([sf_bbox_shift], [sf_bbox_expand], [sf_bbox_contract])
#'
#' The only functions with additional parameters are sf_bbox_to_lonlat_query and sf_bbox_dist. All
#' other functions only take a bbox parameter.
#'
#' @param bbox A bounding box object.
#' @param coords query column names with coordinates. e,g, c("X", "Y") or
#'   c("lat", "lon")
#' @param crs coordinate reference system to use for query; default 4326
#' @param from,to xy pairs (e.g. c("xmax", "ymax) defining points to measure
#'   distance from and to.
#' @param units If `TRUE`, distance functions return with units. If `FALSE`
#'   (default), distance functions return numeric values.
#' @param orientation If `TRUE`, sf_bbox_asp returns a suggested orientation
#'   based on aspect ratio (< 0.9 "portrait"; > 1.1 "landscape"; else "square");
#'   defaults to `FALSE`.
#' @param x_nudge,y_nudge Length 1 or 2 numeric vector; unitless.
#' @param side one or more sides to shift: "top", "bottom", "left", "right", or
#'   "all"
#' @param dir If "in", contract the `bbox` by x_nudge and y_nudge. If "out",
#'   expand the bbox by x_nudge and y_nudge. If dir is not `NULL`; absolute
#'   values are used for x_nudge and y_nudge. Defaults to `NULL`.
#' @md
#' @name sf_bbox_misc
NULL

#' @name sf_bbox_asp
#' @rdname sf_bbox_misc
#' @export
sf_bbox_asp <- function(bbox, orientation = FALSE) {
  xdist <- sf_bbox_xdist(bbox) # Get width
  ydist <- sf_bbox_ydist(bbox) # Get height
  bbox_asp <- xdist / ydist # Get width to height aspect ratio for bbox
  if (!orientation) {
    return(bbox_asp)
  } else {
    if (bbox_asp > 1.1) {
      bbox_orientation <- "landscape"
    } else if (bbox_asp < 0.9) {
      bbox_orientation <- "portrait"
    } else {
      bbox_orientation <- "square"
    }
    return(bbox_orientation)
  }
}

#' @name sf_bbox_dist
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_crs st_point st_distance
#' @importFrom units as_units
sf_bbox_dist <- function(bbox, from, to, units = FALSE) {
  dist <-
    sf::st_distance(
      sf::st_point(c(bbox[[from[1]]], bbox[[from[2]]])),
      sf::st_point(c(bbox[[to[1]]], bbox[[to[2]]]))
    )

  dist <- as.numeric(dist)

  if (units) {
    bbox_units <- sf::st_crs(bbox)$units_gdal
    dist <- units::as_units(dist, value = bbox_units)
  }

  return(dist)
}

#' @name sf_bbox_xdist
#' @rdname sf_bbox_misc
#' @export
sf_bbox_xdist <- function(bbox, units = FALSE) {
  # abs(bbox["xmax"] - bbox["xmin"]) # Get width
  sf_bbox_dist(
    bbox = bbox,
    from = c("xmin", "ymin"),
    to = c("xmax", "ymin"),
    units = units
  )
}

#' @name sf_bbox_ydist
#' @rdname sf_bbox_misc
#' @export
sf_bbox_ydist <- function(bbox, units = FALSE) {
  # abs(bbox["ymax"] - bbox["ymin"]) # Get height
  sf_bbox_dist(
    bbox = bbox,
    from = c("xmin", "ymin"),
    to = c("xmin", "ymax"),
    units = units
  )
}

#' @name sf_bbox_diagdist
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_bbox st_distance st_point
sf_bbox_diagdist <- function(bbox, units = FALSE) {
  sf_bbox_dist(
    bbox = bbox,
    from = c("xmin", "ymin"),
    to = c("xmax", "ymax"),
    units = units
  )
}

#' @name sf_bbox_transform
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_crs st_transform st_bbox
sf_bbox_transform <- function(bbox, crs) {
  if(!check_sf_same_crs(bbox, crs)) {
    bbox_sf <- sf::st_transform(sf_bbox_to_sf(bbox), crs)
    bbox <- sf::st_bbox(bbox_sf)
  }
  return(bbox)
}

#' @param bbox bbox object
#' @return sf object
#' @seealso
#'  - \code{\link[sf]{st_as_sf}},\code{\link[sf]{st_as_sfc}}
#'  - \code{\link[sfx]{st_extent}}
#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_as_sf st_as_sfc
sf_bbox_to_sf <- function(bbox) {
  sf::st_as_sf(sf::st_as_sfc(bbox))
}

#' @rdname sf_bbox_misc
#' @export
#' @importFrom sf st_as_text st_as_sfc
sf_bbox_to_wkt <- function(bbox) {
  # Convert bbox to well known text
  sf::st_as_text(sf::st_as_sfc(bbox))
}

#' @rdname sf_bbox_misc
#' @export
#' @importFrom glue glue
#' @importFrom sf st_transform st_bbox
sf_bbox_to_lonlat_query <- function(bbox, coords = c("longitude",  "latitude"), crs = 4326) {

  bbox <- sf_bbox_transform(bbox, crs = crs)

  # FIXME: This automatic reversal needs to be documented
  if (grepl("lat|Y|y", coords[1])) {
    coords <- rev(coords)
  }

  glue::glue("({coords[1]} >= {bbox$xmin[[1]]}) AND ({coords[1]} <= {bbox$xmax[[1]]}) AND {coords[2]} >= {bbox$ymin[[1]]}) AND ({coords[2]} <= {bbox$ymax[[1]]})")
}


#' @rdname sf_bbox_misc
#' @name sf_bbox_shift
#' @export
sf_bbox_shift <- function(bbox,
                          x_nudge = 0,
                          y_nudge = 0,
                          side = c("all", "top", "bottom", "left", "right"),
                          dir = NULL) {
  side <- match.arg(side, several.ok = TRUE)

  if (is.character(dir)) {
    dir <-
      switch(dir,
             "in" = c(1, -1),
             "out" = c(-1, 1)
      )
  } else {
    dir <- c(dir * -1, dir)
  }

  if ((length(x_nudge) == 1) && length(y_nudge) == 1) {
    if (is.null(dir)) {
      x_nudge <-
        list(
          "min" = x_nudge,
          "max" = x_nudge
        )

      y_nudge <-
        list(
          "min" = y_nudge,
          "max" = y_nudge
        )
    } else if (is.numeric(dir)) {
      x_nudge <- abs(x_nudge)
      y_nudge <- abs(y_nudge)

      x_nudge <-
        list(
          "min" = x_nudge * dir[[1]],
          "max" = x_nudge * dir[[2]]
        )

      y_nudge <-
        list(
          "min" = y_nudge * dir[[1]],
          "max" = y_nudge * dir[[2]]
        )
    }
  } else if ((length(x_nudge) == 2) && (length(y_nudge) == 2)) {
    x_nudge <-
      list(
        "min" = x_nudge[[1]],
        "max" = x_nudge[[2]]
      )

    y_nudge <-
      list(
        "min" = y_nudge[[1]],
        "max" = y_nudge[[2]]
      )
  }

  check_side <- function(x, side_opts) {
    any(c(x, "all") %in% side_opts)
  }

  if (check_side("left", side)) {
    bbox[["xmin"]] <- bbox[["xmin"]] + x_nudge[["min"]]
  }

  if (check_side("right", side)) {
    bbox[["xmax"]] <- bbox[["xmax"]] + x_nudge[["max"]]
  }

  if (check_side("bottom", side)) {
    bbox[["ymin"]] <- bbox[["ymin"]] + y_nudge[["min"]]
  }

  if (check_side("top", side)) {
    bbox[["ymax"]] <- bbox[["ymax"]] + y_nudge[["max"]]
  }

  return(bbox)
}

#' @rdname sf_bbox_misc
#' @name sf_bbox_contract
#' @export
sf_bbox_contract <- function(bbox,
                             x_nudge = 0,
                             y_nudge = 0) {


  sf_bbox_shift(bbox = bbox,
                x_nudge = x_nudge,
                y_nudge = y_nudge,
                side = "all",
                dir = -1
  )

}

#' @rdname sf_bbox_misc
#' @name sf_bbox_expand
#' @export
sf_bbox_expand <- function(bbox,
                           x_nudge = 0,
                           y_nudge = 0) {

  sf_bbox_shift(bbox = bbox,
                x_nudge = x_nudge,
                y_nudge = y_nudge,
                side = "all",
                dir = 1
  )

}

