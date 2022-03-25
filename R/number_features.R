#' Sort and number features
#'
#' Used with [layer_number_markers]
#'
#' @param data Marker data
#' @param groupname_col Group column name, Default: `NULL`
#' @param sort_col Sort column name, Default: 'lon'
#' @param number If TRUE, number the markers, Default: `TRUE`
#' @param desc If `TRUE`, sort descending; set to `FALSE` if sort is "lon" and
#'   TRUE is sort is "lat" or any other value, default `NULL`
#' @return A sf object with a number column ordered by sort.
#' @rdname number_markers
#' @export
#' @importFrom dplyr arrange mutate row_number
number_features <- function(data, col = NULL, number = TRUE, sort = "lon", desc = NULL, ...) {
  number_col <- "number"

  sort <- match.arg(sort, c("lon", "lat", "title", "label", names(data)))

  # Set defaults for desc that make sense for the northern hemisphere
  # TODO: document this or make it an option to reverse
  if (is.null(desc)) {
    if (sort == "lon") {
      desc <- FALSE
    } else if (sort == "lat") {
      desc <- TRUE
    } else {
      desc <- FALSE
    }
  }

  if ((sort %in% c("lat", "lon")) && !all(c("lat", "lon") %in% names(data))) {
    data <-
      st_coords(
        data,
        geometry = "centroid",
        drop = FALSE
      )
  }

  if (!is.null(col)) {
    data <- group_by_col(data, col = col, ...)
    by_group <- TRUE
  } else {
    by_group <- FALSE
  }

  if (desc) {
    data <-
      dplyr::arrange(data, dplyr::desc(.data[[sort]]), .by_group = by_group)
  } else {
    data <-
      dplyr::arrange(data, .data[[sort]], .by_group = by_group)
  }

  if (number) {
    data <-
      dplyr::mutate(data, number = dplyr::row_number(), .before = dplyr::everything())
  }

  return(data)
}
