
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

  if (rlang::has_name(params, "col")) {
    params$location_col <- params$col
    params$data_col <- params$col
  }

  location <- as_sf_list(x = location, col = params$location_col)
  data <- as_sf_list(x = data, col = params$data_col)

  len_location <- length(location)
  len_data <- length(data)

  location_data_list <-
    dplyr::case_when(
      (len_location == 1) && (len_data > 1) ~ list(rep(location, len_data), data),
      (len_data == 1) && (len_location > 1) ~ list(location, rep(data, len_location)),
      TRUE ~ list(location, data)
    )

  if (!(len_location == len_data) && (len_location != 1) && (len_data != 1)) {
    cli::cli_warn("location is length {location_len} and data is {data_len}.")
  }

  if (!is.null(key) && (length(key) == 2)) {
    names(location_data_list) <- key
  }

  location_data_list
}


#' @noRd
get_location_data_list <- function(data = NULL, location = NULL, nm = NULL, ...) {
  # FIXME: Should this be titled map_location_data_list?
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

  make_location_data_list(data, location)
}
