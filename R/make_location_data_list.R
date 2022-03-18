
#' Make a list of data and corresponding locations
#'
#' This function converts data and location into lists of sf objects using
#' [make_sf_list]. If location_col, data_col, or col (which sets both to the
#' same value), are provided the col is passed to [make_sf_list] to allow the
#' creation of an sf list from a sf data frame using [dplyr::group_nest()].
#'
#' If location and data are the same length are the same length, they are
#' combined into a single list. If either one is length 1 when the other is not,
#' the length 1 object is repeated to match the length of the longer object.
#' Different length objects where neither are length 1 return an error.
#'
#' @param data,location A sf object or list of sf objects with data and
#'   corresponding locations.
#' @param key Names for location and data in the returned list.
#' @param ... Pass location_col and/or data_col to group and nest the data
#'   provided to location and data. Use col to set both to the same value.
#' @export
make_location_data_list <- function(data, location, key = c("location", "data"), ...) {
  params <- rlang::list2(...)

  if ("col" %in% names(params)) {
    params$location_col <- params$col
    params$data_col <- params$col
  }

  if (is.null(params$location_col)) {
    location <- make_sf_list(location)
  } else if ("location_col" %in% names(params)) {
    location <- make_sf_list(x = location, col = params$location_col)
  }

  len_location <- length(location)

  if (is.null(params$data_col)) {
    data <- make_sf_list(x = data)
  } else if ("data_col" %in% names(params)) {
    data <- make_sf_list(x = data, col = params$data_col)
  }

  len_data <- length(data)

  if (len_location == len_data) {
    location_data_list <- list(location, data)
  } else if (len_location == 1) {
    location_data_list <- list(rep(location, len_data), data)
  } else if (len_data == 1) {
    location_data_list <- list(location, rep(data, len_location))
  } else {
    usethis::ui_stop("location and data must be either length 1 or the same length as one another.")
  }

  if (!is.null(key) && (length(key) == 2)) {
    names(location_data_list) <- key
  }

  return(location_data_list)
}

#' Make a simple feature list with an optional grouping column
#'
#' @param x an sf object or a data frame with a sf list column named "data"
#' @param nm name(s) for sf list; defaults to "data". If col is provided, the
#'   values of the grouping column are used as names.
#' @param col Grouping column must be a named varialable
#' @export
as_sf_list <- function(x, nm = "data", col = NULL) {
  stopifnot(
    is.null(col) || (is.character(col) && (length(col) == 1))
  )

  if (!overedge::is_sf_list(x)) {
    if ((is.data.frame(data)) && ("data" %in% names(data)) && overedge::is_sf_list(data$data)) {
      # data frame with nested list column named data
      # produced by group_nest w/ keep_all = TRUE

      if (is.character(col)) {
        nm <- dplyr::summarize(x, .data[[col]])[[1]]
      } else if (nm == "data") {
        nm <- NULL
      }

      x <- x$data
    } else if (overedge::is_sf(x)) {
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
    overedge::is_sf_list(x)
  )

  if (is.null(names(x)) && !is.null(nm)) {
    names(x) <- janitor::make_clean_names(nm)
  }

  return(x)
}
