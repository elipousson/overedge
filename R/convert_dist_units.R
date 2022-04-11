#' @noRd
is_units <- function(x) {
  is_class(x, "units")
}

#' @noRd
is_same_units <- function(x, y) {
  as.character(units(x)) == as.character(units(y))
}

#' Convert distances between different units
#'
#' @param dist Numeric or units
#' @param from Existing unit for dist, Default: NULL. If dist is a units object, the numerator is used as "from"
#' @param to Unit to convert distance to, Default: 'meter'
#' @return Object created by [units::set_units()]
#' @rdname convert_dist_units
#' @export
#' @importFrom units set_units
convert_dist_units <- function(dist,
                               from = NULL,
                               to = "meter") {
  stopifnot(
    is.numeric(dist) || is_units(dist)
  )

  if (is_units(dist)) {
    from <- get_dist_units(dist)
  }

  if (!is.null(from)) {
    from <- match.arg(from, dist_unit_options)

    if (!is_units(dist)) {
      from <- gsub(" ", "_", from)
      dist <-
        units::set_units(
          x = dist,
          value = from,
          mode = "standard"
        )
    }
  }

  if (!is.null(to)) {
    to <- gsub(" ", "_", to)
    to <- match.arg(to, dist_unit_options)

    dist <-
      units::set_units(
        x = dist,
        value = to,
        mode = "standard"
      )
  }

  return(dist)
}

#' General utility functions for working with distance units objects
#'
#' - is_dist_units: Is x a distance unit object?
#' - get_dist_units: Get distance units from x (works for sf and units objects)
#' - is_same_dist, is_shorter, is_longer: Compare units objects
#'
#' @param x object to check
#' @name is_dist_units
is_dist_units <- function(x) {
  is_units(x) && (get_dist_units(x) %in% dist_unit_options)
}

#' @name get_dist_units
#' @rdname is_dist_units
get_dist_units <- function(x, null.ok = TRUE) {
  if (is.null(x) && null.ok) {
    return(x)
  } else if (is.null(x)) {
    cli::cli_abort(
      "{.var units} must be a unit chracter string, a unit class object, or a sf object with a valid coordinate reference system."
    )
  }

  if (is_sf(x)) {
    return(sf::st_crs(x)$units_gdal)
  }

  if (is_units(x)) {
    return(as.character(units(x)[["numerator"]]))
  }

  if (is.character(x)) {
    return(x[x %in% dist_unit_options])
  }
}

#' @name as_dist_units
#' @rdname is_dist_units
#' @importFrom sf st_crs
#' @importFrom units as_units
as_dist_units <- function(x, units = NULL, null.ok = FALSE) {
  units <- get_dist_units(units, null.ok = null.ok)

  if (!is.null(units)) {
    units <- match.arg(units, dist_unit_options)
  } else if (null.ok) {
    return(x)
  }

  if (is.numeric(x) && !is_dist_units(x)) {
    units::as_units(x, units)
  } else if (cli_yeah("Did you mean to convert {.var x} to {.val {units}}?")) {
    convert_dist_units(
      dist = x,
      to = units
    )
  }
}

#' @name diff_dist
#' @rdname is_dist_units
#' @importFrom dplyr case_when
#' @importFrom cli cli_alert_danger
is_diff_dist <- function(x, y, units = NULL) {
  which_is_units <-
    dplyr::case_when(
      is_units(x) && !is_units(y) ~ "x",
      !is_units(x) && is_units(y) ~ "y",
      is_units(x) && is_units(y) ~ "xy",
      TRUE ~ "neither"
    )

  if ((which_is_units == "neither") && is.null(units)) {
    cli::cli_alert_danger("No units could be determined for x or y.")
  }

  switch(which_is_units,
    "x" = diff(c(x, as_dist_units(y, units = x))),
    "y" = diff(c(as_dist_units(x, units = y), y)),
    "xy" = diff(c(x, y)),
    "neither" = diff(c(as_units(x, units = units), as_units(y, units = units)))
  )
}

#' @name is_same_dist
#' @rdname  is_dist_units
#' @importFrom sf st_area
#' @export
is_same_dist <- function(x, y, dist = NULL, diff = FALSE, ...) {
  if (is.character(dist) && is_sf(x, ext = TRUE) && is_sf(y, ext = TRUE)) {
    x <- as_bbox(x)
    y <- as_bbox(x)

    dist <- match.arg(dist, c("diagdist", "xdist", "ydist"))

    x <-
      switch(dist,
        # FIXME: Is this going to work or is there a tolerance factor needed?
        "diagdist" = sf_bbox_diagdist(x, drop = FALSE),
        "xdist" = sf_bbox_xdist(x, drop = FALSE),
        "ydist" = sf_bbox_ydist(x, drop = FALSE)
      )

    y <-
      switch(dist,
        # FIXME: Is this going to work or is there a tolerance factor needed?
        "diagdist" = sf_bbox_diagdist(y, drop = FALSE),
        "xdist" = sf_bbox_xdist(y, drop = FALSE),
        "ydist" = sf_bbox_ydist(y, drop = FALSE)
      )
  }

  if (diff) {
    return(is_diff_dist(x, y))
  }

  all.equal(as.numeric(is_diff_dist(x, y)), 0, ...)
}

#' @name is_longer
#' @rdname is_dist_units
is_longer <- function(x, y) {
  as.numeric(is_diff_dist(x, y)) > 0
}

#' @name is_shorter
#' @rdname is_dist_units
is_shorter <- function(x, y) {
  as.numeric(is_diff_dist(x, y)) < 0
}
