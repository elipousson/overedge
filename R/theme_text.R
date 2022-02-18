#' Set, update, or replace text family for ggplot theme and geoms
#'
#' @param font_family Font family, Default: 'Helvetica' If NULL, font_family is
#'   pulled from current set theme which is helpful for resetting all text
#'   families to the theme.
#' @param method ggplot2 function to use in changing text "set", "update", or
#'   "replace", Default: 'set'
#' @param geom If TRUE, update text family for geom_text, geom_sf_text,
#'   geom_label, and geom_sf_label to match `font_family`. If FALSE, make no changes to the theme. Default: TRUE
#' @seealso
#' \code{\link[ggplot2]{theme}},\code{\link[ggplot2]{theme_get}},\code{\link[ggplot2]{update_geom_defaults}}
#' @rdname theme_text
#' @export
#' @importFrom ggplot2 theme element_text theme_set theme_update theme_replace
#'   update_geom_defaults
theme_text <- function(font_family = "Helvetica",
                       color = "black",
                       method = NULL,
                       geom = TRUE) {
  if (is.null(font_family)) {
    font_family <- ggplot2::theme_get()$text$family
  }

  theme_font_family <-
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = font_family, color = color),
      plot.subtitle = ggplot2::element_text(family = font_family, color = color),
      plot.caption = ggplot2::element_text(family = font_family, color = color),
      strip.text = ggplot2::element_text(family = font_family, color = color),
      axis.text = ggplot2::element_text(family = font_family, color = color),
      axis.title = ggplot2::element_text(family = font_family, color = color),
      legend.text = ggplot2::element_text(family = font_family, color = color),
      legend.title = ggplot2::element_text(family = font_family, color = color)
    )

  if (!is.null(method)) {
    method <- match.arg(method, c("set", "update", "replace"))

    switch(method,
      "set" = ggplot2::theme_set(
        theme_font_family
      ),
      "update" = ggplot2::theme_update(
        theme_font_family
      ),
      "replace" = ggplot2::theme_replace(
        theme_font_family
      )
    )

    if (geom_text) {
      ggplot2::update_geom_defaults(
        "label",
        list(family = font_family, color = color)
      )
      ggplot2::update_geom_defaults(
        "text",
        list(family = font_family, color = color)
      )
    }
  } else {
    theme_font_family
  }
}


#' Set, update, or replace margins for ggplot theme
#'
#' @inheritParams get_paper
#' @inheritParams theme_text
#' @param fill fill for plot.background passed to ggplot2::element_rect(), Default: NA
#' @param color color for plot.background passed to ggplot2::element_rect(), Default: NA
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ggplot() +
#'     layer_loc
#'   theme_margin(paper = "letter")
#'
#'   theme_margin(margin = "narrow", paper = "letter")
#' }
#' }
#' @seealso
#'  \code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}}
#' @rdname theme_margin
#' @export
#' @importFrom ggplot2 theme element_rect
theme_margin <- function(margin = "standard",
                         paper = NULL,
                         orientation = NULL,
                         dist = NULL,
                         unit = "in",
                         plot_width = NULL,
                         header = 0,
                         footer = 0,
                         fill = NA,
                         color = NA,
                         size = 0,
                         method = NULL) {
  theme_margins <-
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = fill,
        color = color,
        size = grid::unit(size, units = unit)
      ),
      plot.margin = get_margin(
        margin = margin,
        paper = paper,
        orientation = orientation,
        dist = dist,
        unit = unit,
        plot_width = plot_width,
        header = header,
        footer = footer
      )
    )

  if (!is.null(method)) {
    method <- match.arg(method, c("set", "update", "replace"))

    switch(method,
      "set" = ggplot2::theme_set(
        theme_margins
      ),
      "update" = ggplot2::theme_update(
        theme_margins
      ),
      "replace" = ggplot2::theme_replace(
        theme_margins
      )
    )
  } else {
    theme_margins
  }
}
