#' Layer arrows
#'
#' <https://paleolimbot.github.io/ggspatial/reference/geom_spatial_segment.html>
#'
#' @param data PARAM_DESCRIPTION
#' @param start_col PARAM_DESCRIPTION, Default: 'start'
#' @param end_col PARAM_DESCRIPTION, Default: 'end'
#' @param type PARAM_DESCRIPTION, Default: 'closed'
#' @seealso
#'  [ggplot2::reexports()], [ggplot2::geom_segment()], [ggplot2::aes()]
#' @rdname layer_arrows
#' @noRd
#' @importFrom ggplot2 arrow geom_curve aes
layer_arrows <- function(data,
                         start_col = "start",
                         end_col = "end",
                         type = "closed") {
  ends <- match.arg(type, c("last", "first", "both"))
  type <- match.arg(type, c("open", "closed"))

  # get_start_end_point
  arrow_spec <-
    ggplot2::arrow(
      angle = angle,
      length = unit(0.03, "npc"),
      ends = ends,
      type = type
    )

  ggplot2::geom_curve(
    ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
    data = df,
    arrow = arrow_spec
  )
}
