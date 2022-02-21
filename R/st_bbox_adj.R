#' Get bounding box buffered and adjusted to match aspect ratio
#'
#' Takes an area as an sf object or a bounding box and returns a bounding box
#' that matches the provided aspect ratio and contains the area or bounding box
#' provided.
#'
#' Common aspect ratios include "1:1" (1), "4:6" (0.666), "8.5:11", "16:9"
#' (1.777). The asp parameter supports both numeric values and character
#' strings with ratios matching the format of "width:height".
#'
#' @param x \code{sf} to adjust
#' @inheritParams st_buffer_ext
#' @inheritParams st_bbox_asp
#' @param crs Coordinate reference system of bounding box to return
#' @return Class \code{bbox} object
#' @export
#' @importFrom sf st_transform st_bbox
st_bbox_adj <- function(x = NULL,
                        dist = NULL,
                        diag_ratio = NULL,
                        asp = NULL,
                        unit = NULL,
                        crs = NULL) {
  if (check_bbox(x)) {
    x <- sf_bbox_to_sf(x)
  }

  # Get buffered area
  x <-
    st_buffer_ext(
      x = x,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit
    )

  if (!is.null(crs)) {
    x <- sf::st_transform(x, crs)
  }

  if (!is.null(asp)) {
    # Get aspect adjusted bbox
    bbox <-
      st_bbox_asp(
        x = x,
        asp = asp
      )
  } else {
    bbox <-
      sf::st_bbox(x)
  }

  return(bbox)
}


#' Get bounding box adjusted to match aspect ratio
#'
#' Takes an area as an  \code{sf} or \code{bbox} object and returns a bounding
#' box that matches the aspect ratio provided to \code{asp} and contains the
#' area or bounding box provided. Common aspect ratios include "1:1" (1), "4:6"
#' (0.666), "8.5:11", "16:9" (1.777). The asp parameter supports both numeric
#' values and character strings with ratios matching the format of
#' "width:height".
#'
#' @param x \code{sf} or bbox object
#' @inheritParams get_asp
#' @return \code{bbox} object
#' @export
#' @importFrom sf st_geometry_type st_bbox
#' @importFrom checkmate test_class
st_bbox_asp <- function(x = NULL,
                        asp = NULL) {
  geom_type <- sf::st_geometry_type(x, by_geometry = FALSE)

  if (check_bbox(x)) {
    bbox <- x
  } else if (grepl("^POINT", geom_type)) {
    x <- st_buffer_ext(x, dist = 1)
    bbox <- sf::st_bbox(x)
  } else {
    bbox <- sf::st_bbox(x)
  }

  # Get adjusted aspect ratio
  asp <- get_asp(asp = asp)

  if (!is.null(asp)) {

    # Get width/height
    xdist <- sf_bbox_xdist(bbox) # Get width
    ydist <- sf_bbox_ydist(bbox) # Get height

    # Set default nudge to 0
    x_nudge <- 0
    y_nudge <- 0

    # Compare adjust aspect ratio to bbox aspect ratio
    if (asp >= sf_bbox_asp(bbox)) {
      # adjust x
      x_nudge <- (asp * ydist - xdist) / 2
    } else {
      # adjust y
      y_nudge <- ((xdist / asp) - ydist) / 2
    }

    # Adjust bbox
    bbox[["xmin"]] <- bbox[["xmin"]] - x_nudge
    bbox[["xmax"]] <- bbox[["xmax"]] + x_nudge
    bbox[["ymin"]] <- bbox[["ymin"]] - y_nudge
    bbox[["ymax"]] <- bbox[["ymax"]] + y_nudge
  }

  return(bbox)
}


#' Get aspect ratio from string or based on specific paper and margins
#'
#'
#' @param asp Aspect ratio of width to height as a numeric value (e.g. 0.33) or
#'   character (e.g. "1:3"). If numeric, [get_asp()] returns the same value
#'   without modification.
#' @inheritParams get_paper
#' @inheritParams get_margin
#' @param block_asp If `TRUE`, get aspect ratio of the map/plot area (not
#'   including the margins); defaults to `FALSE`.
#' @return A numeric aspect ratio.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   get_asp("1:2")
#'
#'   get_asp(11 / 17)
#'
#'   get_asp(paper = "letter")
#' }
#' }
#' @rdname get_asp
#' @export
#' @importFrom stringr str_detect str_extract
#' @importFrom usethis ui_stop
get_asp <- function(asp = NULL,
                    paper = NULL,
                    orientation = NULL,
                    margin = NULL,
                    unit = NULL,
                    block_asp = FALSE) {
  if (is.null(paper)) {
    # Check aspect ratio
    if (is.character(asp) && grepl(":", asp)) {
      # If asp is provided as character string (e.g. "16:9") convert to a numeric ratio
      asp <-
        as.numeric(stringr::str_extract(asp, ".+(?=:)")) /
        as.numeric(stringr::str_extract(asp, "(?<=:).+"))
    } else if (!is.numeric(asp) && !is.null(asp)) {
      usethis::ui_stop("{usethis::ui_value('asp')} must be numeric (e.g. 0.666) or a string representing a width to height ratio (e.g. '4:6').")
    }
  } else if (!is.null(paper) && is.null(asp)) {
    # Get aspect ratio for text/plot/map block area
    paper <- get_paper(paper = paper, orientation = orientation)

    if (block_asp) {
      if (is.null(unit)) {
        unit <- paper$units
      }

      # Get margins and convert to numeric (note substitute original value of paper for paper$name)
      margin <- get_margin(margin = margin, paper = paper$name, orientation = orientation, unit = unit)
      margin <- as.numeric(margin)

      # Calculate width, height, and aspect ratio for text/plot/map block area
      block_width <- paper$width - (margin[[2]] + margin[[4]])
      block_height <- paper$height - (margin[[1]] + margin[[3]])
      asp <- block_width / block_height
    } else {
      asp <- paper$asp
    }
  }

  return(asp)
}
