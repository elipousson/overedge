#' @noRd
#' @importFrom ggplot2 theme_set theme_update theme_replace
theme_method <- function(x, method = NULL) {
  method <- match.arg(method, c("set", "update", "replace"))

  switch(method,
    "set" = ggplot2::theme_set(
      x
    ),
    "update" = ggplot2::theme_update(
      x
    ),
    "replace" = ggplot2::theme_replace(
      x
    )
  )
}

#' Modify the text, margins, or legend for a ggplot theme
#'
#' @param font_family Font family, Default: 'Helvetica' If NULL, font_family is
#'   pulled from current set theme which is helpful for resetting all text
#'   families to the theme.
#' @param geom_text If TRUE, update text family for geom_text, geom_sf_text,
#'   geom_label, and geom_sf_label to match `font_family` and color. If FALSE,
#'   make no changes to the theme. Default: TRUE
#' @param fill fill for plot.background passed to ggplot2::element_rect(),
#'   Default: NA
#' @param color color for text elements (passed to ggplot2::element_text by
#'   theme_text), plot.background (passed to ggplot2::element_rect() by
#'   theme_margin). Default: NA
#' @param position position type or a a two-element numeric vector
#' @param margin margin distance, a margin style supported by get_margins or a
#'   margin object, Default: 10
#' @param unit margin units, Default: 'pt'
#' @param inset If TRUE and position is "topleft", "bottomleft", "topright", or
#'   "bottomright", place the legend in an inset position. Default: TRUE
#' @param bgcolor fill color for legend background, Default: 'white'
#' @param justification If NULL, justification is set to "center", Default: NULL
#' @param method ggplot2 function to use in changing text "set", "update", or
#'   "replace", Default: 'set'
#' @inheritParams get_paper
#' @inheritParams get_margin
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   theme_margin(paper = "letter")
#'
#'   theme_margin(margin = "narrow", paper = "letter")
#' }
#' }
#' @seealso
#'  \code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}},\code{\link[ggplot2]{theme_get}},\code{\link[ggplot2]{update_geom_defaults}
#' @name theme_ext
NULL

#' @rdname theme_ext
#' @name theme_text
#' @export
#' @importFrom ggplot2 theme element_text theme_set theme_update theme_replace
#'   update_geom_defaults
theme_text <- function(font_family = NULL,
                       color = "black",
                       geom_text = TRUE,
                       method = NULL) {
  if (is.null(font_family)) {
    font_family <- ggplot2::theme_get()$text$family
  }

  text_theme <-
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
    theme_method(text_theme, method = method)

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
    return(text_theme)
  }
}

#' @rdname theme_ext
#' @name theme_margin
#' @export
#' @importFrom ggplot2 theme element_rect
#' @importFrom grid unit
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
  margin_theme <-
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
    theme_method(margin_theme, method = method)
  } else {
    return(margin_theme)
  }
}

#' @rdname theme_ext
#' @name theme_legend
#' @export
#' @importFrom ggplot2 element_blank element_rect theme
#' @importFrom grid unit
theme_legend <- function(position,
                         margin = 10,
                         unit = "pt",
                         inset = TRUE,
                         bgcolor = "white",
                         justification = NULL,
                         method = NULL) {

  # If margin is not a unit object
  if (!("unit" %in% class(margin))) {
    # use a numeric margin as a dist
    if (is.numeric(margin)) {
      margin <- get_margin(dist = margin, unit = unit)
    } else if (is.character(margin)) {
      # use a character margin as a margin type
      margin <- get_margin(margin = margin, unit = unit)
    }
  }

  if (is.null(bgcolor)) {
    bg <- ggplot2::element_blank()
  } else {
    bg <- ggplot2::element_rect(fill = bgcolor)
  }

  if (!is.numeric(position)) {
    position <- match.arg(position, c(
      "none", "left", "right", "bottom", "top",
      "topleft", "bottomleft", "topright", "bottomright"
    ))
  }


  if (inset) {
    if (grepl("top", position)) {
      y_position <- 0.95
      y_justification <- "top"
    } else if (grepl("bottom", position)) {
      y_position <- 0.05
      y_justification <- "bottom"
    }

    if (grepl("left", position)) {
      x_position <- 0.05
      x_justification <- "left"
      box_just <- "left"
    } else if (grepl("right", position)) {
      x_position <- 0.95
      x_justification <- "right"
    }

    position <- grid::unit(c(x_position, y_position), unit = "npc")
    justification <- c(x_justification, y_justification)
  } else if (is.null(justification)) {
    justification <- "center"
  }

  legend_theme <- ggplot2::theme(
    legend.position = position,
    legend.justification = justification,
    legend.margin = margin,
    legend.background = bg
  )


  if (!is.null(method)) {
    theme_method(legend_theme, method = method)
  } else {
    return(legend_theme)
  }
}