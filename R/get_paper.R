
#' Get standard paper and image sizes
#'
#' Use the "paper" parameter (matching name from [paper_sizes]), standard
#' (optionally including series and size) parameter, or width, height and
#' units. May return multiple paper sizes depending on parameters.
#'
#' @param paper Paper, Default: 'letter'.
#' @param orientation Orientation "portrait", "landscape", or "square", Default:
#'   'portrait'.
#' @param standard Size standard, "ANSI", "ISO", "British Imperial", "JIS",
#'   "USPS", "Facebook", "Instagram", or "Twitter".
#' @param series Size series (e.g. A), Default: `NULL`
#' @param size Size number (only used for "ISO" and "JIS" series). Standard,
#'   series, and size may all be required to return a single paper when using
#'   these parameters.
#' @param width Width in units, Default: `NULL`.
#' @param height Height in units, Default: `NULL`.
#' @param units Paper size units, either "in", "mm", or "px"; defaults to `NULL`
#'   (using "in" if width or height are provided).
#' @return Data frame with one or more paper/image sizes.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   get_paper("letter")
#'
#'   get_paper(paper = NULL, standard = "ISO", series = "A", size = 4)
#'
#'   get_paper(width = 11, height = 17)
#' }
#' }
#' @rdname get_paper
#' @export
#' @importFrom dplyr filter rename
#' @importFrom rlang .data
get_paper <- function(paper = "letter",
                      orientation = "portrait",
                      standard = NULL,
                      series = NULL,
                      size = NULL,
                      width = NULL,
                      height = NULL,
                      units = NULL) {
  orientation <- match.arg(orientation, c("portrait", "landscape", "square"), several.ok = TRUE)

  if (!is.null(standard) | !is.null(width)) {
    paper <- NULL
  }

  if (!is.null(paper)) {
    paper <- dplyr::filter(
      paper_sizes,
      tolower(.data$name) %in% tolower(paper)
    )
  } else if (!is.null(standard)) {
    paper_standard <- match.arg(standard, c("ANSI", "ISO", "British Imperial", "JIS", "USPS", "Facebook", "Instagram", "Twitter"), several.ok = TRUE)
    paper <- dplyr::filter(
      paper_sizes,
      .data$standard %in% paper_standard
    )

    if (!is.null(series)) {
      paper_series <- match.arg(series, c("A", "B", "C", "Engineering", "Architecture", "EDDM"), several.ok = TRUE)

      paper <- dplyr::filter(
        paper,
        .data$series %in% paper_series
      )

      if (!is.null(size)) {
        paper_size <- size

        paper <- dplyr::filter(
          paper,
          .data$size %in% paper_size
        )
      }
    }
  } else if (!is.null(width)) {
    paper_width <- width

    units <- match.arg(units, c("in", "mm", "px"))
    paper_units <- units

    paper <- dplyr::filter(
      paper_sizes,
      .data$width %in% paper_width,
      .data$units %in% paper_units
    )

    if (!is.null(height)) {
      paper_height <- height

      paper <- dplyr::filter(
        paper_sizes,
        .data$height %in% paper_height
      )
    }
  }

  # Save width and height before checking orientation
  width <- paper$width
  height <- paper$height

  if (orientation == "portrait") {
    paper <-
      dplyr::rename(
        paper,
        asp = asp_portrait
      )
  } else if (orientation == "landscape") {
    paper$width <- height
    paper$height <- width

    paper <-
      dplyr::rename(
        paper,
        asp = asp_landscape
      )
  } else if (orientation == "square") {
    paper <-
      dplyr::rename(
        paper,
        asp = asp_portrait
      )
  }

  return(paper)
}

#' Get margins for a ggplot2 plot or map based on style or distance
#'
#' This function works in combination with the [get_paper()] function to make it
#' easier to position a map on a page before saving to file. This is primarily
#' useful when using a map or plot created with ggplot2 as part of a print
#' document format that is composed outside of R using a page layout application
#' such as Adobe InDesign.
#'
#' @param margin Margin style (options include "extrawide", "wide", "standard",
#'   "narrow", "none"), Additional "auto" option to generate margin based on
#'   line length is planned but not yet implemented. Default: `NULL` (equivalent to "none").
#' @param dist Margin distance (single value used to all sides), Default: `NULL`
#' @param unit Unit for margin distance, Default: 'in'.
#' @param plot_width Plot or map width in units. If `paper` and `plot_width` are
#'   provided, margins are half the distance between the two evenly distributed.
#'   This is not tested and may not work with all page sizes/orientations.
#' @param header Header height in units; defaults to 0.
#' @param footer Footer height in units; defaults to 0.
#' @inheritParams get_paper
#' @return A \code{\link[ggplot2]{margin}} element intended for use with
#'   \code{\link[ggplot2]{element_rect}} and the `plot.background` theme element.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   get_margin("standard")
#'
#'   get_margin("none")
#'
#'   get_margin(dist = 25, unit = "mm")
#'
#'   get_margin(paper = "letter", plot_width = 5.5)
#' }
#' }
#' @seealso
#'  \code{\link[ggplot2]{margin}}
#' @rdname get_margin
#' @export
#' @importFrom ggplot2 margin
#' @importFrom grid unit
get_margin <- function(margin = NULL,
                       paper = NULL,
                       orientation = NULL,
                       dist = NULL,
                       unit = "in",
                       plot_width = NULL,
                       header = 0,
                       footer = 0) {
  margin <- match.arg(margin, c("none", "narrow", "standard", "extrawide", "wide"))
  unit <- match.arg(unit, c("in", "mm", "px", "cm", "npc", "picas", "pc", "pt", "lines", "char", "native"))

  if (!is.null(paper)) {
    paper <- get_paper(paper = paper, orientation = orientation)

    if (!is.null(plot_width)) {
      dist <- (paper$width - plot_width) / 2
    }
  }

  if (is.null(dist)) {
    if (is.character(margin) && (margin != "auto")) {
      if (unit == "in") {
        margin <- switch(margin,
          "extrawide" = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = unit),
          "wide" = ggplot2::margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = unit),
          "standard" = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = unit),
          "narrow" = ggplot2::margin(t = 0.75, r = 0.75, b = 0.75, l = 0.75, unit = unit),
          "none" = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = unit)
        )
      } else if (unit == "mm") {
        margin <- switch(margin,
          "extrawide" = ggplot2::margin(t = 80, r = 80, b = 80, l = 80, unit = unit),
          "wide" = ggplot2::margin(t = 60, r = 60, b = 60, l = 60, unit = unit),
          "standard" = ggplot2::margin(t = 40, r = 40, b = 40, l = 40, unit = unit),
          "narrow" = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = unit),
          "none" = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = unit)
        )
      } else if (unit == "px" && !is.null(paper)) {
        px_to_npc_margins <- function(px) {
          list(
            "t" = 1 - (px / paper$height),
            "r" = 1 - (px / paper$width),
            "b" = (px / paper$height),
            "l" = (px / paper$width)
          )
        }

        margin <- switch(margin,
          "extrawide" = ggplot2::margin(px_to_npc_margins(120), unit = "npc"), # 1080 / 6
          "wide" = ggplot2::margin(px_to_npc_margins(80), unit = "npc"), # 1080 / 8
          "standard" = ggplot2::margin(px_to_npc_margins(40), unit = "npc"), # 1080 / 12
          "narrow" = ggplot2::margin(px_to_npc_margins(20), unit = "npc"),
          "none" = ggplot2::margin(px_to_npc_margins(0), unit = "npc")
        )
      }
    } else if (margin == "auto") {
      # TODO: implement margin settings that respond to font family and size and/or paper
      # e.g. LaTeX default is 1.5 in margin w/ 12pt text, 1.75 in for 11pt, 1.875 for 10pt
      # See https://practicaltypography.com/page-margins.html for more information on linelength and margins
    }
  } else if (unit != "px") {
    if (length(dist) == 1) {
      margin <- ggplot2::margin(t = dist, r = dist, b = dist, l = dist, unit = unit)
    } else if (length(dist) == 4) {
      margin <- ggplot2::margin(t = dist[[1]], r = dist[[2]], b = dist[[3]], l = dist[[4]], unit = unit)
    }
  }

  if (unit != "px") {
    margin <- margin + grid::unit(x = c(header, 0, 0, footer), unit = unit)
  }

  return(margin)
}

#' Get aspect ratio from string or based on specific paper and margins
#'
#'
#' @param asp Aspect ratio of width to height as a numeric value (e.g. 0.33) or
#'   character (e.g. "1:3"). If numeric, [get_asp()] returns the same value
#'   without modification.
#' @param margin A margin style supported by [get_margins()], a numeric vector
#'   (length 1 or 4) passed to dist parameter of get_margins, or a margins
#'   object.
#' @param block_asp If `TRUE`, return the aspect ratio of a text, map, or plot
#'   block on the paper when using the specified margins.
#' @inheritParams get_paper
#' @inheritParams get_margin
#' @param block_asp If `TRUE`, get aspect ratio of the map/plot area (not
#'   including the margins); defaults to `FALSE`.
#' @return A numeric aspect ratio.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   get_asp("1:2")
#'
#'   get_asp(11 / 17)
#'
#'   get_asp(paper = "letter")
#' }
#' }
#' @rdname get_asp
#' @export
#' @importFrom stringr str_detect str_extract
#' @importFrom usethis ui_stop
get_asp <- function(asp = NULL,
                    paper = NULL,
                    orientation = NULL,
                    margin = NULL,
                    unit = NULL,
                    block_asp = FALSE) {
  if (is.null(paper)) {
    # Check aspect ratio
    if (is.character(asp) && grepl(":", asp)) {
      # If asp is provided as character string (e.g. "16:9") convert to a numeric ratio
      asp <-
        as.numeric(stringr::str_extract(asp, ".+(?=:)")) /
          as.numeric(stringr::str_extract(asp, "(?<=:).+"))
    } else if (!is.numeric(asp) && !is.null(asp)) {
      usethis::ui_stop("{usethis::ui_value('asp')} must be numeric (e.g. 0.666) or a string representing a width to height ratio (e.g. '4:6').")
    }
  } else if (!is.null(paper) && is.null(asp)) {
    # Get aspect ratio for text/plot/map block area
    paper <- get_paper(paper = paper, orientation = orientation)

    if (block_asp) {
      if (is.null(unit)) {
        unit <- paper$units
      }

      # Get margins and convert to numeric (note substitute original value of paper for paper$name)
      if (is.character(margin)) {
        margin <- get_margin(margin = margin, paper = paper$name, orientation = orientation, unit = unit)
      } else if (is.numeric(margin)) {
        margin <- get_margin(dist = margin, unit = unit)
      } else if (!check_class(margin, check = "margin")) {
        usethis::ui_stop("margin must be either a character string matching the margin options (“none”, “narrow”, “standard”, “wide”, or “extrawide”), a numeric vector that can be passed to the dist parameter of get_margins, or a margin class object.")
      }

      margin <- as.numeric(margin)

      # Calculate width, height, and aspect ratio for text/plot/map block area
      block_width <- paper$width - (margin[[2]] + margin[[4]])
      block_height <- paper$height - (margin[[1]] + margin[[3]])
      asp <- block_width / block_height
    } else {
      asp <- paper$asp
    }
  }

  return(asp)
}


#' Get standard scales and convert to scale distances
#' @name get_standard_scale
#' @param standard USGS, Engineering, or Architectural
#' @param series Map series
#' @param scale Scale name
#' @param convert If `TRUE`, return paper with scale distances converted to actual distances based on values from selected scale; defaults to `FALSE`.
#' @inheritParams get_paper
#' @export
get_standard_scale <- function(standard = NULL,
                               series = NULL,
                               scale = NULL,
                               paper = NULL,
                               orientation = NULL,
                               convert = FALSE) {

  select_scale <- standard_scales

  if (!is.null(scale)) {
    select_scale <- dplyr::filter(select_scale, .data$scale %in% {{ scale }})
  }

  if (!is.null(standard)) {
    standard <- match.arg(standard, c("USGS", "Engineering", "Architectural"), several.ok = TRUE)
    select_scale <- dplyr::filter(select_scale, .data$standard %in% {{ standard }})
  }

  if (!is.null(series)) {
    series <- match.arg(series, unique(standard_scales$series), several.ok = TRUE)
    select_scale <- dplyr::filter(select_scale, .data$series %in% {{ series }})
  }

  if (is.null(paper) && !convert) {

    return(select_scale)

  } else if (!is.null(paper) && convert) {
    paper <-
      get_paper(paper = paper, orientation = orientation)

  #  accuracy <- match.arg(accuracy, c("exact", "approximate"), several.ok = TRUE)

    paper$scale <- select_scale$scale

  if (paper$units == "mm") {

    # Converted to cm
    scale_width <- paper$width / 10
    scale_height <- paper$height / 10
    scale_units <- "cm"

    # Distance of 1 cm
    unit_actual <- select_scale$scale_cm_unit
    unit_dist <- select_scale$scale_cm

   #scale_factor <-
  #    switch(actual_unit,
  #      "m" = 100,
  #      "km" = 100000,
  #      "ft" = 30.48,
  #      "feet" = 30.48,
  #      "mi" = 160934
  #    )
  } else if (paper$units == "in") {

    scale_width <- paper$width
    scale_height <- paper$height
    scale_units <- paper$units

    unit_actual <- select_scale$scale_in_unit
    unit_dist <- select_scale$scale_in

   # scale_factor <-
  #    switch(unit_actual,
  #           "m" = 39.3701,
  #           "km" = 39370.1,
  #           "ft" = 12,
  #           "feet" = 12,
  #           "mi" = 63360
  #    )
  }

  actual_width <- scale_width * unit_dist #* scale_factor
  actual_height <- scale_height * unit_dist # * scale_factor)

  if (unit_actual %in% c("ft", "feet")) {
    actual_width <- units::set_units(
      x = actual_width,
      value = "feet"
    )

    actual_height <- units::set_units(
      x = actual_height,
      value = "feet"
    )
  } else if (unit_actual == "mi") {
    actual_width <- units::set_units(
      x = actual_width,
      value = "mi"
    )

    actual_height <- units::set_units(
      x = actual_height,
      value = "mi"
    )
  } else if (unit_actual == "m") {
    actual_width <- units::set_units(
      x = actual_width,
      value = "m"
    )

    actual_height <- units::set_units(
      x = actual_height,
      value = "m"
    )
  } else if (unit_actual == "km") {
    actual_width <- units::set_units(
      x = actual_width,
      value = actual_unit
    )

    actual_height <- units::set_units(
      x = actual_height,
      value = "km"
    )
  }

  paper$unit_actual <- unit_actual
  paper$width_actual <- actual_width
  paper$height_actual <- actual_height

  return(paper)
  }
}
