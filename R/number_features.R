#' Sort and number features
#'
#' Used with [layer_numbers()]
#'
#' @param data Marker data
#' @param col Group column name, Default: `NULL`
#' @param sort Sort column name, Default: 'lon'
#' @param desc If `TRUE`, sort descending; set to `FALSE` if sort is "lon" and
#'   TRUE is sort is "lat" or any other value, default `NULL`
#' @param num_style Style of enumeration, either "arabic", "alph", "Alph", "roman",
#'   "Roman"
#' @param suffix Character to appended to "number" column.
#' @return A sf object with a number column ordered by sort.
#' @export
#' @importFrom dplyr mutate row_number everything
#' @importFrom utils as.roman
#' @importFrom rlang has_name
#' @importFrom cli cli_warn
number_features <- function(data,
                            col = NULL,
                            sort = "lon",
                            desc = NULL,
                            num_style = "arabic",
                            suffix = NULL) {

  if (rlang::has_name(data, "number")) {
    cli::cli_warn(
      "number_features is replacing an existing column with the name number."
    )
  }

  data <-
    sort_features(
      data,
      col = col,
      sort = sort,
      desc = desc
    )

  data <-
    dplyr::mutate(
      data,
      number = dplyr::row_number(),
      .before = dplyr::everything()
    )

  num_style <- match.arg(num_style, c("arabic", "alph", "Alph", "roman", "Roman"))

  data$number <- switch(num_style,
    "arabic" = data$number,
    "alph" = tolower(sapply(data$number, int_to_alph)),
    "Alph" = toupper(sapply(data$number, int_to_alph)),
    "roman" = tolower(utils::as.roman(data$number)),
    "Roman" = toupper(utils::as.roman(data$number))
  )

  data$number <- paste0(data$number, suffix)

  return(data)
}

#' Adapted from https://stackoverflow.com/a/44274075
#'
#' @noRd
int_to_alph <- function(num, suffix = NULL, base = 26) {
  rest <- (num - 1) %/% base

  suffix <-
    paste0(
      LETTERS[((num - 1) %% base) + 1],
      suffix
    )

  if (rest > 0) {
    return(Recall(rest, base, suffix))
  }

  return(suffix)
}

#' @name sort_features
#' @rdname number_features
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr arrange desc
sort_features <- function(data,
                          col = NULL,
                          sort = "lon",
                          desc = NULL) {

  coords <- c("lon", "lat")
  sort <- match.arg(sort, c(coords, names(data)))

  # Set defaults for desc that make sense for the northern hemisphere
  # TODO: document this or make it an option to reverse
  if (is.null(desc)) {
    desc <- FALSE

    if (sort == "lat") {
      desc <- TRUE
    }
  }

  if ((sort %in% coords) && !all(rlang::has_name(data, coords))) {
    data <-
      st_coords(
        data,
        coords = coords,
        geometry = "centroid",
        drop = FALSE
      )
  }

  by_group <- FALSE

  if (!is.null(col)) {
    data <- group_by_col(data, col = col)
    # FIXME: Is there a reason to allow grouping by column even if not numbering by group
    by_group <- TRUE
  }

  if (desc) {
    data <-
      dplyr::arrange(data, dplyr::desc(.data[[sort]]), .by_group = by_group)
  } else {
    data <-
      dplyr::arrange(data, .data[[sort]], .by_group = by_group)
  }

  return(data)
}
