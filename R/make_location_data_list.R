
#' Make a list of data and corresponding locations
#'
#' This function converts data and location into lists of sf objects using
#' [as_sf_list]. If location_col, data_col, or col (which sets both to the
#' same value), are provided the col is passed to [as_sf_list] to allow the
#' creation of an sf list from a sf data frame using [dplyr::group_nest()].
#'
#' If location and data are the same length are the same length, they are
#' combined into a single list. If either one is length 1 when the other is not,
#' the length 1 object is repeated to match the length of the longer object.
#' Different length objects where neither are length 1 gives a warning.
#'
#' @param data,location A sf object or list of sf objects with data and
#'   corresponding locations.
#' @param key Names for location and data in the returned list.
#' @param ... Pass location_col and/or data_col to group and nest the data
#'   provided to location and data. Use col to set both to the same value.
#' @export
make_location_data_list <- function(data = NULL, location = NULL, key = c("location", "data"), ...) {
  stopifnot(
    !is.null(data) && !is.null(location)
  )

  params <- rlang::list2(...)

  if ("col" %in% names(params)) {
    params$location_col <- params$col
    params$data_col <- params$col
  }

  if (is.null(params$location_col)) {
    location <- as_sf_list(location)
  } else if ("location_col" %in% names(params)) {
    location <- as_sf_list(x = location, col = params$location_col)
  }

  len_location <- length(location)

  if (is.null(params$data_col)) {
    data <- as_sf_list(x = data)
  } else if ("data_col" %in% names(params)) {
    data <- as_sf_list(x = data, col = params$data_col)
  }

  len_data <- length(data)

  if ((len_location == 1) && (len_data > 1)) {
    location_data_list <- list(rep(location, len_data), data)
  } else if ((len_data == 1) && (len_location > 1)) {
    location_data_list <- list(location, rep(data, len_location))
  } else {
    if (!(len_location == len_data)) {
      cli::cli_warn("location is length {location_len} and data is {data_len}.")
    }

    location_data_list <- list(location, data)
  }

  if (!is.null(key) && (length(key) == 2)) {
    names(location_data_list) <- key
  }

  return(location_data_list)
}


#' @noRd
get_location_data_list <- function(data = NULL, location = NULL, nm = NULL, ...) {
  if (is.null(nm) && !is.null(data)) {
    nm <- names(data)
  } else if (is.null(nm) && !is.null(location)) {
    nm <- names(location)
  }

  stopifnot(
    (length(data) == 1) || (length(location) == 1) || (length(location) == length(data))
  )

  # Get data for multiple locations
  if (length(data) == 1) {
    if (is.list(data)) {
      data <- data[[1]]
    }

    data <-
      purrr::map(
        location,
        ~ get_location_data(
          location = .x,
          data = data,
          ...
        )
      )
  } else if ((length(location) == 1)) {
    if (is.list(location)) {
      location <- location[[1]]
    }

    # Get multiple data for a single locations
    data <-
      purrr::map(
        data,
        ~ get_location_data(
          location = location[[1]],
          data = .x,
          ...
        )
      )
  } else if (length(location) == length(data)) {
    data <-
      purrr::map2(
        location,
        data,
        ~ get_location_data(
          location = .x,
          data = .y,
          ...
        )
      )
  }

  location_data_list <- make_location_data_list(data, location)

  return(location_data_list)
}
