#' Convert an object to a simple feature or bounding box object
#'
#' Both functions will pass a NULL value without returning an error. If a POINT
#' or MULTIPOINT object is passed to [as_bbox] a 0.00000001 meter buffer is
#' applied.
#'
#' @param x A `sf`, `bbox`, `sfc`, `raster`, `sp`, or data frame object that can
#'   be converted into a simple feature or bounding box object. [as_bbox()] can
#'   also convert a vector with xmin, ymin, xmax, and ymax values. [as_sf_list]
#'   only supports sf objects or a data frames with a sf list column named
#'   "data" (typically created by using [dplyr::group_nest()] on an sf object.
#' @param sf_col A column name to use for the geometry column created by
#'   [as_sf]; defaults to "geometry".
#' @param crs Coordinate reference system for `sf`, `bbox`, `sfc` or `sf` list
#'   object to return.
#' @param ... Additional parameters passed to [sf::st_bbox()] when calling
#'   [as_bbox()] or passed to [sf::st_sf()], [sf::st_as_sf()], or [df_to_sf()]
#'   for [as_sf()] (depending on class of x)
#' @export
#' @importFrom sf st_sf st_as_sfc st_bbox st_as_sf st_geometry
#' @importFrom dplyr bind_rows rename
as_sf <- function(x, crs = NULL, sf_col = "geometry", ...) {
  if (is_sf(x)) {
    return(as_crs(x, crs = crs))
  }

  # Convert objects to sf if needed
  x_is <-
    dplyr::case_when(
      is_bbox(x) ~ "bbox",
      is_sfg(x) ~ "sfg",
      is_sfc(x) ~ "sfc",
      is_sf_list(x) ~ "sf_list",
      is.data.frame(x) ~ "df",
      is_raster(x) ~ "raster",
      is_sp(x) ~ "sp"
    )

  x <-
    switch(x_is,
      "bbox" = sf_bbox_to_sf(x, ...),
      "sfg" = sf::st_sf(sf::st_sfc(x), ...),
      "sfc" = sf::st_sf(x, ...),
      "sf_list" = dplyr::bind_rows(x),
      "df" = df_to_sf(x, ...),
      "raster" = sf::st_sf(sf::st_as_sfc(sf::st_bbox(x)), ...),
      "sp" = sf::st_as_sf(x, ...)
    )

  if (!is.null(sf_col) && (attributes(x)$sf_column != sf_col)) {
    sf::st_geometry(x) <- sf_col
  }

  return(as_crs(x, crs = crs))
}

#' @name as_bbox
#' @rdname as_sf
#' @export
#' @importFrom sf st_bbox st_as_sf
#' @importFrom dplyr bind_rows
#' @importFrom rlang has_length
as_bbox <- function(x, crs = NULL, ...) {
  if (is_bbox(x)) {
    return(sf_bbox_transform(bbox = x, crs = crs))
  }

  # Convert objects to sf if needed
  x_is <-
    dplyr::case_when(
      is_geom_type(x, type = c("POINT", "MULTIPOINT")) ~ "sf_pt",
      is_sf(x, ext = TRUE) ~ "sf_or_sfc",
      rlang::has_length(x, 4) && all(is.numeric(x)) ~ "num_bbox",
      TRUE ~ "other"
    )

  x <-
    switch(x_is,
      "sf_pt" = sf::st_bbox(st_buffer_ext(x, dist = 0.00000001), ...),
      "sf_or_sfc" = sf::st_bbox(x, ...),
      "num_bbox" = sf::st_bbox(c(
        xmin = x[1], ymin = x[2],
        xmax = x[3], ymax = x[4]
      ),
      crs = crs, ...
      ),
      "other" = sf::st_bbox(as_sf(x), ...)
    )

  return(sf_bbox_transform(bbox = x, crs = crs))
}


#' @name as_sfc
#' @rdname as_sf
#' @export
#' @importFrom sf st_geometry st_as_sfc
as_sfc <- function(x, crs = NULL, ...) {
  if (is_sfc(x)) {
    return(as_crs(x, crs = crs))
  }

  x_is <-
    dplyr::case_when(
      is_sf(x) ~ "sf",
      is_sfg(x) ~ "sfg",
      TRUE ~ "other"
    )

  x <-
    switch(x_is,
      "sfg" = sf::st_sfc(x, ...),
      "sf" = sf::st_geometry(x, ...),
      "other" = sf::st_geometry(as_sf(x, ...))
    )

  x <- as_crs(x, crs = crs)

  return(x)
}

#' @noRd
#' @importFrom sf st_crs st_transform
as_crs <- function(x, crs = NULL, ...) {
  if (is.null(crs)) {
    return(x)
  }

  if (is_sf(crs, ext = TRUE)) {
    crs <- sf::st_crs(crs)
  }

  if (is.na(sf::st_crs(x))) {
    sf::st_crs(x) <- crs
    return(x)
  }

  if (is_same_crs(x, crs)) {
    return(x)
  }

  sf::st_transform(x, crs = crs, ...)
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

    # data frame with nested list column named data
    # produced by group_nest w/ keep_all = TRUE
    if (is.data.frame(x) && (rlang::has_name(x, "data")) && is_sf_list(x$data)) {
      if (nm == "data") {
        nm <- NULL
      }

      if (is.character(col)) {
        nm <- dplyr::summarize(x, .data[[col]])[[1]]
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
    if (is_sf(crs, ext = TRUE)) {
      crs <- sf::st_crs(crs)
    }

    # FIXME: This sets up the possibility of an error if the sf list is bounding
    # boxes
    if (x_is_sf || x_is_sf_list) {
      x <- purrr::map(x, ~ as_crs(.x, crs = crs))
    }
  }

  return(x)
}

#' Convert data to a different class
#'
#' @param data Data that can be converted to sf, sfc, bbox or a sf list object.
#' @param class A class to convert data to; defaults to NULL (which returns
#'   "sf")
#' @param crs coordinate reference system
#' @noRd
as_sf_class <- function(x, class = NULL, crs = NULL, ...) {
  if (is.null(class)) {
    return(x)
  }

  class <- match.arg(class, c("sf", "sfc", "bbox", "list"))

  x <-
    switch(class,
      "sf" = as_sf(x, crs = crs, ...),
      "sfc" = as_sfc(x, crs = crs, ...),
      "bbox" = as_bbox(x, crs = crs, ...),
      "list" = as_sf_list(x, crs = crs, ...)
    )

  return(x)
}


#' Convert a simple feature object or a numeric string to a single POINT (sfg)
#' or POINT geometry set (sfc)
#'
#' Works with sf, sfc, and bbox objects using [sf::st_centroid]. Works with
#' [sf_bbox_point]
#'
#' @rdname as_points
#' @name as_point
#' @export
as_points <- function(...) {
  params <- rlang::list2(...)

  crs <- NULL

  if (rlang::has_name(params, "crs")) {
    crs <- params$crs
    params <- params[names(params) != "crs"]
  }
  pts <-
    purrr::map(
      params,
      ~ as_point(.x)
    )

  if (is.null(crs)) {
    x <- sf::st_as_sfc(pts)
  } else {
    x <- sf::st_as_sfc(pts, crs = crs)
  }

  # See https://github.com/r-spatial/sf/issues/114
  sf::st_cast(x, to = "POINT")
}

#' @rdname as_points
#' @name as_point
#' @export
as_point <- function(...) {
  params <-
    rlang::list2(...)

  if (length(params) == 1) {
    params <- params[[1]]
  }

  if (is_sfg(params)) {
    params <- as_sfc(params)
  }

  if (is_sf(params, ext = TRUE)) {
    if (is_bbox(params)) {
      params <- as_sfc(params)
    }

    params <- sf::st_union(params)

    return(sf::st_centroid(params)[[1]])
  }

  if (any(sapply(params, is_bbox))) {
    return(rlang::exec(sf_bbox_point, !!!params))
  }

  sf::st_point(params)
}
