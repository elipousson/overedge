#' Make a grid over a location bounding box
#'
#' Create a grid with an id column and optionally a set number of columns and
#' rows.
#'
#' @param location A `sf`, `sfc`, or `bbox` object, Default: `NULL`. Required.
#' @param cols,rows Used to set n if either are not `NULL`; defaults to `NULL`.
#'   row and id are added as columns to the grid if they are provided.
#' @param gutter Distance in units between each column cell; gutter effectively
#'   serves as a margin as the negative buffer is applied to all cells
#'   (including those at the edges of the grid).
#' @param rev If `TRUE`, id, row, and col numbers are assigned left to right and
#'   top to bottom.
#' @inheritParams st_bbox_ext
#' @param n If n is NULL and square is `TRUE`, the grid is set automatically to
#'   be 10 cells wide, Default: `NULL`
#' @param ... Additional parameters passed to [sf::st_make_grid]
#' @inheritParams sf::st_make_grid
#' @example examples/make_location_grid.R
#' @seealso [sf::st_make_grid]
#' @rdname make_location_grid
#' @export
#' @importFrom rlang list2
#' @importFrom sf st_make_grid st_filter
#' @importFrom dplyr mutate row_number
make_location_grid <- function(location = NULL,
                               dist = NULL,
                               diag_ratio = NULL,
                               asp = NULL,
                               unit = "meter",
                               crs = NULL,
                               cols = NULL,
                               rows = NULL,
                               gutter = 0,
                               desc = FALSE,
                               n = NULL,
                               cellsize = NULL,
                               what = NULL,
                               style = "rect",
                               .id = "id") {
  stopifnot(
    !is.null(location)
  )

  style <- match.arg(style, c("rect", "square", "hex", "flat_top_hex", "circle", "circle_offset"))

  # Get adjusted bounding box using any adjustment variables provided
  bbox <-
    st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs
    )

  params <-
    get_grid_params(
      bbox = bbox,
      cellsize = cellsize,
      unit = unit,
      n = n,
      what = what,
      cols = cols,
      rows = rows,
      style = style
    )

  bbox_sf <- as_sf(bbox)

  grid <-
    sf::st_make_grid(
      x = bbox_sf,
      cellsize = params$cellsize,
      n = params$n,
      what = params$what,
      square = params$square,
      flat_topped = params$flat_topped
    )

  grid <- as_sf(grid)

  grid <- sf::st_filter(x = grid, y = bbox_sf)

  if (style %in% c("rect", "square", "circle")) {
    grid <-
      dplyr::mutate(
        grid,
        col = rep(sort(seq(params$cols), decreasing = desc), params$rows),
        row = sort(rep(seq(params$rows), params$cols), decreasing = !desc)
      )

    grid <-
      dplyr::arrange(grid, row, col)
  }

  grid <-
    dplyr::mutate(
      grid,
      "{.id}" := dplyr::row_number(),
      .before = dplyr::everything()
    )

  grid <- relocate_sf_col(grid)

  if (style %in% c("circle", "circle_offset")) {
    grid <-
      st_buffer_ext(
        grid,
        dist = (sf_bbox_xdist(bbox) / params$cols) / 2
      )
  }

  if (!is.null(gutter) && (gutter != 0)) {
    grid <-
      st_buffer_ext(
        x = grid,
        dist = abs(gutter) * -0.5,
        unit = unit
      )
  }

  return(grid)
}

#' @noRd
get_grid_params <- function(bbox,
                            cellsize = NULL,
                            unit = NULL,
                            n = NULL,
                            what = NULL,
                            cols = NULL,
                            rows = NULL,
                            base = 10,
                            style = NULL) {
  what <- match.arg(what, c("polygons", "corners", "centers"))
  style <- match.arg(style, c("rect", "square", "hex", "flat_top_hex", "circle", "circle_offset"))

  if (!is.null(cellsize)) {
    if (rlang::has_length(n, 1)) {
      cellsize <- rep(cellsize, 2)
    }

    if (!is.null(cols) && is_longer(cols * cellsize[1], sf_bbox_xdist(bbox))) {
      cli::cli_alert_danger("The cellsize will not fit within the width of the bounding box with the number of columns requested.")
    }

    if (!is.null(rows) && is_longer(rows * cellsize[2], sf_bbox_ydist(bbox))) {
      cli::cli_alert_danger("The specified cellsize will not fit within the height of the bounding box with the number of rows requested.")
    }
  }

  bbox_asp <- sf_bbox_asp(bbox)

  if (is.null(n) && is.null(cols) && is.null(rows)) {
    cols <- base
    rows <- base
  }

  if (is.null(n) && !is.null(cellsize)) {
    diff_bbox <- as.numeric(c(diff(bbox[c(1, 3)]), diff(bbox[c(2, 4)])))
    n <- diff_bbox / cellsize
  }

  if (is.null(n) && is.null(cellsize)) {
    n <-
      dplyr::case_when(
        (!is.null(cols) && (style == "square")) ~ c(cols, cols / bbox_asp),
        (!is.null(cols) && is.null(rows)) ~ c(cols, cols),
        (!is.null(cols) && !is.null(rows)) ~ c(cols, rows)
      )

    n <-
      dplyr::case_when(
        (is.null(cols) && !is.null(rows) && (style == "square")) ~ c(rows * bbox_asp, rows),
        (is.null(cols) && !is.null(rows)) ~ c(rows, rows),
        TRUE ~ n
      )

    if (style %in% c("hex", "flat_top_hex")) {
      cli::cli_alert_info("rows and columns do not work consistently with hexagon grids.")
    }
  } else if (!is.null(n)) {
    n <-
      dplyr::case_when(
        rlang::has_length(n, 1) && (style == "square") ~ c(n, n / sf_bbox_asp(bbox)),
        rlang::has_length(n, 1) ~ c(n, n)
      )
  }

  if (is.null(cellsize)) {
    diff_bbox <- as.numeric(c(diff(bbox[c(1, 3)]), diff(bbox[c(2, 4)])))
    cellsize <- diff_bbox / unique(n)
  }

  if (style %in% c("rect", "square", "circle")) {
    square <- TRUE
  } else if (style %in% c("hex", "flat_top_hex", "circle_offset")) {
    square <- FALSE
  }

  if (style == "flat_top_hex") {
    flat_topped <- TRUE
  } else {
    flat_topped <- FALSE
  }

  if (style %in% c("circle", "circle_offset")) {
    what <- "centers"
  }

  list(cellsize = cellsize, n = n, cols = n[1], rows = n[2], square = square, what = what, flat_topped = flat_topped)
}
