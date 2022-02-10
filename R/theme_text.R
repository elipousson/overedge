#' @title Set, update, or replace ggplot theme and geom text family
#'
#' @param font_family Font family, Default: 'Helvetica' If NULL, font_family is
#'   pulled from current set theme which is helpful for resetting all text
#'   families to the theme.
#' @param method ggplot2 function to use in changing text "set", "update", or
#'   "replace", Default: 'set'
#' @param label If TRUE, update default text family for geom_text, geom_sf_text,
#'   geom_label, and geom_sf_label. If FALSE, update default text family for
#'   geoms to match current theme. Default: TRUE
#' @seealso
#' \code{\link[ggplot2]{theme}},\code{\link[ggplot2]{theme_get}},\code{\link[ggplot2]{update_geom_defaults}}
#' @rdname theme_text
#' @export
#' @importFrom ggplot2 theme element_text theme_set theme_update theme_replace
#'   update_geom_defaults
theme_text <- function(font_family = "Helvetica",
                       method = "set",
                       label = FALSE) {
  method <- match.arg(method, c("set", "update", "replace"))

  if (is.null(font_family)) {
   font_family <- ggplot2::theme_get()$text$family
  }

  theme_font_family <-
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = font_family),
      plot.subtitle = ggplot2::element_text(family = font_family),
      plot.caption = ggplot2::element_text(family = font_family),
      strip.text = ggplot2::element_text(family = font_family),
      axis.text = ggplot2::element_text(family = font_family),
      axis.title = ggplot2::element_text(family = font_family),
      legend.text = ggplot2::element_text(family = font_family),
      legend.title = ggplot2::element_text(family = font_family)
    )

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

  if (label) {
    ggplot2::update_geom_defaults(
      "label",
      list(family = font_family)
    )
    ggplot2::update_geom_defaults(
      "text",
      list(family = font_family)
    )
  } else {
    ggplot2::update_geom_defaults(
      "label",
      list(family = ggplot2::theme_get()$text$family)
    )
    ggplot2::update_geom_defaults(
      "text",
      list(family = ggplot2::theme_get()$text$family)
    )
  }
}
