#' Convert an object to a simple feature or bounding box object
#'
#' Both functions will pass a NULL value without returning an error. If a POINT
#' or MULTIPOINT object is passed to [as_bbox] a 0.00000001 meter buffer is
#' applied.
#'
#' @param x A `sf`, `bbox`, `sfc`, `raster`, `sp`, or data frame object that
#'   can be converted into a simple feature or bounding box object. [as_bbox()]
#'   can also convert a vector with xmin, ymin, xmax, and ymax values.
#'   [as_sf_list] only supports sf objects or a data frames with a sf list
#'   column named "data" (typically created by using [dplyr::group_nest()] on an
#'   sf object.
#' @param crs Coordinate reference system for `sf`, `bbox`, `sfc` or `sf` list object to
#'   return.
#' @param ... Additional parameters passed to [sf::st_bbox()] when calling
#'   [as_bbox()] or passed to [sf::st_sf()], [sf::st_as_sf()], or [df_to_sf()]
#'   for [as_sf()] (depending on class of x)
#' @export
#' @importFrom sf st_sf st_as_sfc st_bbox st_as_sf st_geometry
#' @importFrom dplyr bind_rows rename
as_sf <- function(x, crs = NULL, sf_col = "geometry", ...) {
  # Convert objects to sf if needed
  if (!is_sf(x)) {
    if (is_bbox(x)) {
      x <- sf_bbox_to_sf(x, ...)
    } else if (is_sfc(x)) {
      x <- sf::st_sf(x, ...)
    } else if (is_raster(x)) {
      x <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(x)), ...)
    } else if (is_sp(x)) {
      x <- sf::st_as_sf(x, ...)
    } else if (is_sf_list(x)) {
      x <- dplyr::bind_rows(x)
    } else if (is.data.frame(x)) {
      # TODO: Need to figure out a way to pass the crs to df_to_sf
      x <- df_to_sf(x, ...)
    }
  }

  if (!is.null(sf_col) && is_sf(x)) {
    sf::st_geometry(x) <- sf_col
  }

  if (!is.null(crs)) {
    x <- sf::st_transform(x, crs = crs)
  }

  return(x)
}

#' @name as_bbox
#' @rdname as_sf
#' @export
#' @importFrom sf st_bbox st_as_sf
#' @importFrom dplyr bind_rows
as_bbox <- function(x, crs = NULL, ...) {
  # Convert objects to sf if needed
  if (!is_bbox(x)) {
    if (is_sf(x, ext = TRUE)) {
      if (is_geom_type(x)$POINTS) {
        x <- st_buffer_ext(x, dist = 0.00000001)
      }

      x <- sf::st_bbox(x, ...)
    } else if (is_raster(x)) {
      x <- sf::st_bbox(x, ...)
    } else if (is_sp(x)) {
      x <- sf::st_bbox(sf::st_as_sf(x, ...))
    } else if (length(x) == 4) {
      x <- sf::st_bbox(c(
        xmin = x[1], ymin = x[2],
        xmax = x[3], ymax = x[4]
      ),
      crs = crs, ...
      )
    } else if (is_sf_list(x)) {
      x <- sf::st_bbox(dplyr::bind_rows(x))
    } else if (is.data.frame(x)) {
      x <- sf::st_bbox(df_to_sf(x, ...))
    }
  }

  x <- sf_bbox_transform(bbox = x, crs = crs)

  return(x)
}


#' @name as_sfc
#' @rdname as_sf
#' @export
#' @importFrom sf st_geometry st_as_sfc
as_sfc <- function(x, crs = NULL, ...) {
  if (is_sf(x)) {
    x <- sf::st_geometry(x, ...)
  } else if (!is_sfc(x)) {
    x <- sf::st_as_sfc(x, ...)
  }

  if (!is.null(crs)) {
    x <- sf::st_transform(x, crs = crs)
  }

  return(x)
}

#' @param nm For [as_sf_list], name(s) for sf list; defaults to "data". If col is provided, the
#'   values of the grouping column are used as names.
#' @param col For [as_sf_list], the name of the column used to group data if x is a sf object or used
#'   to group and nest data before passing to x.
#' @name as_sf_list
#' @rdname as_sf
#' @export
#' @importFrom dplyr summarize group_keys group_nest
#' @importFrom janitor make_clean_names
as_sf_list <- function(x, nm = "data", col = NULL, crs = NULL) {
  stopifnot(
    is.null(col) || (is.character(col) && (length(col) == 1)),
    !is.null(x)
  )

  x_is_sf <- is_sf(x)
  x_is_sf_list <- is_sf_list(x, ext = TRUE)

  if (!x_is_sf_list) {
    if ((is.data.frame(x)) && ("data" %in% names(x)) && is_sf_list(x$data)) {
      # data frame with nested list column named data
      # produced by group_nest w/ keep_all = TRUE

      if (is.character(col)) {
        nm <- dplyr::summarize(x, .data[[col]])[[1]]
      } else if (nm == "data") {
        nm <- NULL
      }

      x <- x$data
    } else if (x_is_sf) {
      if (is.null(col)) {
        x <- list(x) # coercible sf object in list length 1
      } else {
        x <- group_by_col(data = x, col = col)
        nm <- dplyr::group_keys(x)[[col]]
        x <- dplyr::group_nest(x, keep = TRUE)
        x <- x$data
      }
    }
  }

  stopifnot(
    is_sf_list(x, ext = TRUE)
  )

  if (is.null(names(x)) && !is.null(nm)) {
    names(x) <- janitor::make_clean_names(nm)
  }

  if (!is.null(crs)) {
    # FIXME: This sets up the possibility of an error if the sf list is bounding boxes
    if (x_is_sf || x_is_sf_list) {
      x <- purrr::map(x, ~ sf::st_transform(.x, crs = crs))
    }
  }

  return(x)
}

#' Convert data to a different class
#'
#' @param data Data that can be converted to sf, sfc, bbox or a sf list object.
#' @param class A class to convert data to; defaults to NULL (which returns "sf")
#' @param crs coordinate reference system
#' @noRd
as_sf_class <- function(x, class = NULL, crs = NULL, ...) {
  class <- match.arg(class, c("sf", "sfc", "bbox", "list"))

  if (class == "sf") {
    x <- as_sf(x, crs = crs, ...)
  } else if (class == "sfc") {
    x <- as_sfc(x, crs = crs, ...)
  } else if (class == "bbox") {
    x <- as_bbox(x, crs = crs, ...)
  } else if (class == "list") {
    x <- as_sf_list(x, crs = crs, ...)
  }

  return(x)
}
