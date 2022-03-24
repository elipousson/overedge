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
                               rev = TRUE,
                               n = NULL,
                               cellsize = NULL,
                               square = TRUE,
                               ...) {
  stopifnot(
    !is.null(location)
  )

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

  bbox_asp <- sf_bbox_asp(bbox)
  # width <- sf_bbox_xdist(bbox)
  # height <- sf_bbox_ydist(bbox)
  has_cols <- !is.null(cols)
  has_rows <- !is.null(rows)

  if (has_cols || has_rows) {
    if (!has_cols) {
      cols <- 1
    }

    if (!has_rows) {
      rows <- 1
    }

    n <- c(cols, rows)
  } else {
    if (is.null(n) && square) {
      n <- c(10, 10 / bbox_asp)
    } else if ((length(n) == 1) && is.numeric(n) && square) {
      n <- c(n, n / bbox_asp)
    } else if (is.null(n)) {
      n <- c(10, 10)
    }
  }

  # params <- rlang::list2(...)

  bbox_sf <- as_sf(bbox)

  grid <-
    sf::st_make_grid(
      x = bbox_sf,
      n = n,
      square = square,
      ...
    )

  grid <- as_sf(grid)

  grid <- sf::st_filter(x = grid, y = bbox_sf)

  grid <- dplyr::mutate(
    grid,
    "id" = dplyr::row_number(),
    .before = geometry
  )

  if (rev) {
    grid <- dplyr::arrange(
      grid,
      dplyr::desc(id)
    )

    grid <- dplyr::mutate(
      grid,
      id = dplyr::row_number(),
      .before = geometry
    )
  }

  if (has_cols || has_rows) {
    grid <- dplyr::mutate(
      grid,
      "col" = rep(seq(cols), rows),
      "row" = sort(rep(seq(rows), cols)),
      .before = geometry
    )

    if (rev) {
      grid <- dplyr::arrange(
        grid,
        row,
        dplyr::desc(col)
      )

      grid$col <- rep(seq(cols), rows)
      grid$id <- seq(cols * rows)
    }
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