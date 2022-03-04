
#' Clip a simple feature or bounding box to a side or corner
#'
#' Clip based on the corner of the object bounding box.
#'
#' @param x `sf` or `bbox` object to clip
#' @param clip Character string describing the part of the area to clip or
#'   remove. Options include c("top", "right", "bottom", "left", "topright",
#'   "bottomright", "bottomleft", "topleft"). If NULL, the area is not clipped
#'   and a full edge can be returned.
#' @param keep Alternate way of defining clip (by naming the section to keep).
#' @param flip Logical. Default FALSE. If TRUE, than the clip area
#'   is kept instead of removed. If keep is provided, flip is automatically set to TRUE.
#' @param dist Numeric. Distance to use for the edge. Default NULL
#'   meters. Use negative values for an inside edge or positive numbers for an
#'   outside edge.
#' @param diag_ratio Alternate way to define edge distance.
#' @return `sf` object clipped based on parameters
#' @export
#' @rdname st_clip
#' @export
#' @importFrom sf st_crs st_difference st_bbox st_sfc st_point st_cast st_sf st_convex_hull st_union st_intersection
#' @importFrom dplyr select
st_clip <- function(x,
                    clip = NULL,
                    keep = NULL,
                    flip = FALSE,
                    dist = NULL,
                    diag_ratio = NULL) {

  # If bbox, convert to sf
  if (check_bbox(x)) {
    x <- sf_bbox_to_sf(x)
  }

  crs <- sf::st_crs(x)
  center <- sf_to_df(st_center(x, ext = FALSE), crs = crs)

  if (!is.null(dist) || !is.null(diag_ratio)) {
    outside_edge <-
      (!is.null(dist) && dist > 0) || (!is.null(diag_ratio) && diag_ratio > 0)
    inside_edge <-
      (!is.null(dist) && dist < 0) || (!is.null(diag_ratio) && diag_ratio < 0)

    x_buffer <-
      dplyr::select(
        st_buffer_ext(
          x = x,
          dist = dist,
          diag_ratio = diag_ratio
        ),
        geometry
      )

    if (outside_edge) {
      x <- suppressWarnings(
        sf::st_difference(x_buffer, x)
      )
    } else if (inside_edge) {
      x <- suppressWarnings(
        sf::st_difference(x, x_buffer)
      )
    }
  }

  bbox <- sf::st_bbox(x)

  if (!is.null(keep)) {
    clip <- keep
    flip <- TRUE
  }

  if (!is.null(clip)) {
    clip <- match.arg(clip, c("top", "right", "bottom", "left", "topright", "bottomright", "bottomleft", "topleft"))

    top <- grepl("top", clip)
    bottom <- grepl("bottom", clip)
    left <- grepl("left", clip)
    right <- grepl("right", clip)

    stopifnot(
      !(top && bottom),
      !(left && right)
    )

    if ((top || bottom) && (left || right)) {
      style <- "corner"
    } else {
      style <- "side"
    }

    make_pts <- function(pts) {
      pts <- sf::st_sfc(
        c(
          sf::st_point(pts[[1]]),
          sf::st_point(pts[[2]]),
          sf::st_point(pts[[3]])
        )
      )
      # See https://github.com/r-spatial/sf/issues/114
      sf::st_cast(pts, to = "POINT")
    }

    h_top <-
      make_pts(list(
        c(bbox$xmin, bbox$ymax),
        c(center$lon, bbox$ymax),
        c(bbox$xmax, bbox$ymax)
      ))

    h_middle <-
      make_pts(list(
        c(bbox$xmin, center$lat),
        c(center$lon, center$lat),
        c(bbox$xmax, center$lat)
      ))

    h_bottom <-
      make_pts(
        list(
          c(bbox$xmin, bbox$ymin),
          c(center$lon, bbox$ymin),
          c(bbox$xmax, bbox$ymin)
        )
      )

    v_left <-
      make_pts(
        list(
          c(bbox$xmin, bbox$ymin),
          c(bbox$xmin, center$lat),
          c(bbox$xmin, bbox$ymax)
        )
      )

    v_middle <-
      make_pts(
        list(
          c(center$lon, bbox$ymin),
          c(center$lon, center$lat),
          c(center$lon, bbox$ymax)
        )
      )

    v_right <-
      make_pts(
        list(
          c(bbox$xmax, bbox$ymin),
          c(bbox$xmax, center$lat),
          c(bbox$xmax, bbox$ymax)
        )
      )

    if (style == "side") {
      if (top) {
        pts <- c(h_top, h_middle)
      } else if (bottom) {
        pts <- c(h_bottom, h_middle)
      } else if (left) {
        pts <- c(v_left, v_middle)
      } else if (right) {
        pts <- c(v_right, v_middle)
      }
    } else if (style == "corner") {
      if (top) {
        pts <- v_middle[2:3]
        if (right) {
          pts <- c(pts, h_top[2:3], h_middle[2:3], v_right[2:3])
        } else if (left) {
          pts <- c(pts, h_top[1:2], h_middle[1:2], v_left[2:3])
        } else {
          pts <- c(pts, h_top[2:3], h_middle[2:3], v_right[2:3])
        }
      } else if (bottom) {
        pts <- v_middle[1:2]
        if (right) {
          pts <- c(pts, h_bottom[2:3], h_middle[2:3], v_right[1:1])
        } else if (left) {
          pts <- c(pts, h_bottom[1:2], h_middle[1:2], v_left[1:2])
        } else {
          pts <- c(pts, h_bottom[2:3], h_middle[2:3], v_right[1:1])
        }
      }
    }

    clip <- sf::st_sf(
      name = "",
      crs = crs,
      geometry = sf::st_sfc(sf::st_convex_hull(sf::st_union(pts)))
    )

    if (flip) {
      x <- suppressWarnings(sf::st_intersection(x, clip))
    } else {
      x <- suppressWarnings(sf::st_difference(x, clip))
    }
  }

  #  x <- dplyr::select(x, tidyselect::all_of(x_names))
  return(x)
}
