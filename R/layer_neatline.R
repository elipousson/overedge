#' Set map limits to sf or bbox object with optional adjustments and panel border
#'
#' Set limits for a map to the bounding box of an x using
#' \code{\link[sf]{coord_sf}}. Optionally, adjust the x size by applying a
#' buffer and/or adjust the aspect ratio of the limiting bounding box to match a
#' set aspect ratio.
#'
#' @inheritParams st_bbox_adj
#' @param unit buffer units; defaults to meter.
#' @param data sf or bbox class object
#' @param crs Coordinate reference system to use for
#'   \code{\link[sf]{coord_sf}}.
#' @param expand Default FALSE. If TRUE, use scale_y_continuous and
#'   scale_x_continuous to expand map extent to provided parameters.
#' @param size size of panel border, Default: 1
#' @param color color of panel border, Default: 'black'
#' @param linetype line type of panel border, Default: 'solid'
#' @param hide_grid logical. If TRUE, hide major grid lines. Default: TRUE
#' @param label_axes label_axes passed to \code{\link[sf]{coord_sf}}, Default:
#'   '----' which hides axes labels.
#' @param ... Additional parameters to pass to \code{\link[sf]{coord_sf}}.
#' @return \code{ggplot2::coord_sf()} function with xlim and ylim parameters
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(ggplot2)
#'
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'   ggplot() +
#'     geom_sf(data = nc) +
#'     layer_neatline(data = nc[1, ], asp = 1, color = "red")
#' }
#' }
#' @seealso
#'  \code{\link[ggplot2]{CoordSf}},\code{\link[ggplot2]{scale_continuous}}
#' @aliases set_map_limits
#' @export
#' @importFrom ggplot2 coord_sf scale_y_continuous scale_x_continuous theme element_rect
layer_neatline <- function(data = NULL,
                           dist = NULL,
                           diag_ratio = NULL,
                           unit = "meter",
                           asp = NULL,
                           crs = NULL,
                           color = "black",
                           size = 1,
                           linetype = "solid",
                           expand = FALSE,
                           hide_grid = TRUE,
                           label_axes = "----",
                           ...) {

  # Pass variables to bbox adjustment function
  bbox <- st_bbox_adj(
    x = data,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    crs = crs
  )

  # Set limits with adjustments using coord_sf
  neatline <-
    ggplot2::coord_sf(
      xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
      ylim = c(bbox[["ymin"]], bbox[["ymax"]]),
      label_axes = label_axes,
      ...
    )

  if (expand) {
    neatline <-
      list(
        neatline,
        ggplot2::theme(
          plot.margin = ggplot2::unit(x = c(0, 0, 0, 0), units = "mm")
        ),
        ggplot2::scale_y_continuous(expand = c(0, 0)),
        ggplot2::scale_x_continuous(expand = c(0, 0))
      )
  }

  if (hide_grid) {
    neatline <-
      list(
        neatline,
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = "transparent")
        )
      )
  }

  if (label_axes == "----") {
    neatline <- list(
      neatline,
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks.length = ggplot2::unit(x = 0, units = "mm"),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      )
    )
  }

  neatline <- list(
    neatline,
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = color, size = size, linetype = linetype, fill = NA)
    )
  )

  neatline
}
