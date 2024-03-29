#' Create map layer with shape framing a simple feature object
#'
#' Wraps [st_circle], [st_square], and [layer_neatline].
#'
#' @param frame Type of framing shape to add, "circle" or "square" around data.
#' @param union If `TRUE`, union data before buffering and creating frame;
#'   defaults to `TRUE`.
#' @inheritParams layer_neatline
#' @inheritParams st_misc
#' @inheritParams st_buffer_ext
#' @param fill Fill color for frame.
#' @param neatline If TRUE, add a neatline to the returned layer.
#' @param ... Additional parameters passed to [layer_location_data]. May include
#'   additional fixed aesthetics (e.g. alpha) or "fn" to apply to the frame
#'   object.
#' @example examples/layer_frame.R
#' @name layer_frame
#' @family layer
#' @export
layer_frame <- function(data,
                        dist = NULL,
                        diag_ratio = NULL,
                        unit = "meter",
                        frame = "circle",
                        scale = 1,
                        rotate = 0,
                        inscribed = FALSE,
                        color = "black",
                        size = 1,
                        linetype = "solid",
                        fill = "white",
                        neatline = TRUE,
                        expand = FALSE,
                        union = TRUE,
                        ...) {
  if (union) {
    data <- sf::st_union(data)
  }

  data <-
    st_buffer_ext(
      x = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit
    )

  frame <- make_frame(x = data, frame = frame, scale = scale, rotate = rotate, inscribed = inscribed)

  frame_layer <-
    layer_location_data(
      data = frame,
      geom = "sf",
      fill = fill,
      color = color,
      linetype = linetype,
      ...
    )

  if (neatline) {
    neatline_layer <-
      layer_neatline(
        data = frame,
        asp = 1,
        bgcolor = "none",
        color = "none",
        expand = expand
      )
  } else {
    neatline_layer <-
      NULL
  }

  list(
    frame_layer,
    neatline_layer
  )
}

#' @inheritParams st_misc
#' @name make_frame
#' @rdname layer_frame
#' @export
make_frame <- function(x,
                       frame = "circle",
                       scale = 1,
                       rotate = 0,
                       inscribed = FALSE,
                       dTolerance = 0) {
  stopifnot(
    is_sf(x, ext = TRUE)
  )

  if (frame == "circle") {
    frame <-
      st_circle(x = x, scale = scale, inscribed = inscribed, dTolerance = dTolerance)
  } else if (frame == "square") {
    frame <-
      st_square(x = x, scale = scale, rotate = rotate, inscribed = inscribed)
  } else {
    frame <- x
  }

  frame
}
