#' Set map limits to a simple feature or bounding box with a specified distance
#' buffer, aspect ratio, and panel border
#'
#' Set limits for a map to the bounding box of an x using
#' [ggplot2::coord_sf()]. Optionally, adjust the x size by applying a
#' buffer and/or adjust the aspect ratio of the limiting bounding box to match a
#' set aspect ratio.
#'
#' @inheritParams st_bbox_ext
#' @param unit Buffer units; defaults to meter.
#' @param data `sf` or `bbox` class object
#' @param crs Coordinate reference system to use for
#'   [ggplot2::coord_sf()].
#' @param expand Default `FALSE.` If `TRUE`, the function adds
#'   [ggplot2::scale_y_continuous()] and
#'   [ggplot2::scale_x_continuous()] to expand the map extent to
#'   provided parameters.
#' @param size Size of panel border, Default: 1
#' @param color Color of panel border, Default: 'black'
#' @param bgcolor Fill color of panel background; defaults to "white". If
#'   "none", panel background is set to ggplot2::element_blank()
#' @param linetype Line type of panel border, Default: 'solid'
#' @param hide_grid If `TRUE`, hide major grid lines. Default: `TRUE`
#' @param label_axes A description of which axes to label passed to
#'   [ggplot2::coord_sf()]; defaults to '----' which hides axes
#'   labels.
#' @param ... Additional parameters to pass to [ggplot2::coord_sf()].
#' @return [ggplot2::coord_sf()] function with xlim and ylim
#'   parameters
#' @example examples/layer_neatline.R
#' @seealso
#'  [ggplot2::CoordSf()]
#' @family layer
#' @aliases set_map_limits
#' @export
#' @importFrom ggplot2 coord_sf scale_y_continuous scale_x_continuous theme element_rect element_blank
layer_neatline <- function(data = NULL,
                           dist = getOption("overedge.dist"),
                           diag_ratio = getOption("overedge.diag_ratio"),
                           unit = getOption("overedge.unit", default = "meter"),
                           asp = getOption("overedge.asp"),
                           crs = getOption("overedge.crs"),
                           color = "black",
                           bgcolor = "white",
                           size = 1,
                           linetype = "solid",
                           expand = FALSE,
                           hide_grid = TRUE,
                           label_axes = "----",
                           ...) {

  # Pass variables to bbox adjustment function
  bbox <- st_bbox_ext(
    x = data,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    crs = crs
  )

  # Set limits with adjustments using coord_sf
  limits <-
    ggplot2::coord_sf(
      xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
      ylim = c(bbox[["ymin"]], bbox[["ymax"]]),
      label_axes = label_axes,
      default = TRUE, # Suppress warning about coordinate system being displaced
      crs = crs,
      ...
    )

  if (expand) {
    expand <-
      list(
        ggplot2::scale_y_continuous(expand = c(0, 0)),
        ggplot2::scale_x_continuous(expand = c(0, 0))
      )
  } else {
    expand <- NULL
  }

  if (hide_grid) {
    hide_grid <-
      list(
        ggplot2::theme(
          panel.grid = ggplot2::element_blank()
        )
      )
  } else {
    hide_grid <- NULL
  }

  if (label_axes == "----") {
    label_axes <-
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.ticks.length = ggplot2::unit(x = 0, units = "mm"),
        axis.line = ggplot2::element_blank()
      )
  } else {
    label_axes <- NULL
  }

  if (is.na(bgcolor) || bgcolor == "none") {
    panel_background <-
      ggplot2::element_blank()

    plot_background <-
      ggplot2::element_blank()
  } else {
    panel_background <-
      ggplot2::element_rect(fill = bgcolor)

    plot_background <-
      ggplot2::element_rect(fill = bgcolor)
  }

  if (is.na(color) || color == "none") {
    panel_border <-
      ggplot2::element_blank()
  } else {
    panel_border <-
      ggplot2::element_rect(color = color, size = size, linetype = linetype, fill = NA)
  }

  panel_theme <-
    ggplot2::theme(
      panel.border = panel_border,
      panel.background = panel_background,
      plot.background = plot_background
    )


  list(
    limits,
    expand,
    hide_grid,
    label_axes,
    panel_theme
  )
}
