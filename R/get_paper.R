
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
#' @param cols,rows Number of expected columns and rows in paper; used to determine row_height and section_asp in paper data frame returned by get_paper if rows or cols is greater than 1; defaults to `NULL`.
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
#' @importFrom dplyr filter rename select mutate
#' @importFrom rlang .data
get_paper <- function(paper = "letter",
                      orientation = "portrait",
                      standard = NULL,
                      series = NULL,
                      size = NULL,
                      width = NULL,
                      height = NULL,
                      units = NULL,
                      cols = 1,
                      rows = 1) {
  orientation <- match.arg(orientation, c("portrait", "landscape", "square"), several.ok = TRUE)

  has_width <- !is.null(width)
  has_height <- !is.null(height)
  has_standard <- !is.null(standard)

  if (has_standard | has_width | has_height) {
    paper <- NULL
  }

  if (!is.null(paper)) {
    paper <- dplyr::filter(
      paper_sizes,
      tolower(.data$name) %in% tolower(paper)
    )
  } else if (has_standard) {
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
  } else if (has_width | has_height) {
    units <- match.arg(units, c("in", "mm", "px"))
    paper_units <- units

    if (has_width) {
      paper_width <- width
      paper <- dplyr::filter(
        paper_sizes,
        .data$width %in% paper_width,
        .data$units %in% paper_units
      )
    }

    if (has_height) {
      paper_height <- height

      paper <- dplyr::filter(
        paper_sizes,
        .data$height %in% paper_height,
        .data$units %in% paper_units
      )
    }
  }

  # Save width and height before checking orientation
  # FIXME: This approach sets orientation globally even if returning multiple paper sizes (this is OK but should be documented)
  paper_width <- paper$width
  paper_height <- paper$height

  if (orientation %in% c("portrait", "square")) {
    paper <-
      dplyr::rename(
        paper,
        asp = asp_portrait
      )
    paper <-
      dplyr::select(paper, -asp_landscape)
  } else if (orientation == "landscape") {
    paper$width <- paper_height
    paper$height <- paper_width

    paper <-
      dplyr::rename(
        paper,
        asp = asp_landscape
      )

    paper <-
      dplyr::select(paper, -asp_portrait)
  }

  if ((cols > 1) || rows > 1) {
    paper <-
      dplyr::mutate(
        paper,
        cols = cols,
        col_width = width / cols,
        rows = rows,
        row_height = height / rows,
        section_asp = col_width / row_height,
        orientation = orientation,
        .after = asp,
      )
    # usethis::ui_info("Based on the provided paper ({paper$name}), orientation, rows, and/or columns, the expected detail map size is {col_width} by {row_height}.")
  } else {
    paper <-
      dplyr::mutate(
        paper,
        section_asp = asp,
        orientation = orientation,
        .after = asp
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
#' @param margin A margin style supported by [get_margin()], a numeric vector
#'   (length 1 or 4) passed to dist parameter of get_margin, or a margins
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
        usethis::ui_stop("margin must be either a character string matching the margin options ('none', 'narrow', 'standard', 'wide', or 'extrawide'),
                         a numeric vector that can be passed to the dist parameter of get_margins,
                         or a margin class object.")
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
#'
#' @name get_scale
#' @aliases get_standard_scale
#' @param scale Scale name
#' @param standard USGS, Engineering, or Architectural
#' @param series Map series
#' @export
#' @importFrom dplyr filter
get_scale <- function(scale = NULL,
                      standard = NULL,
                      series = NULL) {
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

  return(select_scale)
}

#' Convert distance from scale to actual units
#'
#' Convert distance from scale to actual units based on named `standard_scales`
#'
#' @param dist distance to convert. If paper is provided, paper width and height
#'   are used as dist.
#' @inheritParams get_paper
#' @inheritParams get_scale
#' @param scale_unit "mm" (converted to cm by dividing by 10), "cm", "px"
#'   (converted to inches by dividing by dpi), or "in".
#' @param actual_unit any unit supported by convert_dist_units
#' @param scale_factor factor for converting from scale_unit to actual_unit, e.g. if 1" = 1', the scale factor is 12. optional if scale if provided; defaults to `NULL`.
#' @param dpi dots per square inch (used as conversion factor for "px" to "in")
#' @return dist values converted from scale_unit to actual_unit based on
#'   scale_factor or information from standard_scales object. If paper is
#'   provided, return a data frame with converted distances as actual_width and
#'   actual_height
#' @export
convert_dist_scale <- function(dist = NULL,
                               paper = NULL,
                               orientation = NULL,
                               scale = NULL,
                               scale_unit = "in",
                               actual_unit = NULL,
                               scale_factor = NULL,
                               dpi = 120) {
  if (!is.null(scale)) {
    scale <- get_scale(scale = scale)
  }

  if (!is.null(paper) && is.null(dist)) {
    paper <- get_paper(paper = paper, orientation = orientation)
    dist <- c(paper$width, paper$height)
    scale_unit <- paper$units
  }

  dist <-
    switch(scale_unit,
      "mm" = dist / 10,
      "cm" = dist,
      # FIXME: Double-check how this handles px
      "px" = dist / dpi,
      "in" = dist
    )

  if (scale_unit %in% c("mm", "cm")) {
    scale_unit <- "cm"
  } else if (scale_unit %in% c("px", "in")) {
    scale_unit <- "in"
  }

  if (is.data.frame(scale)) {
    actual_unit <-
      scale[[paste0("scale_", scale_unit, "_unit")]]

    scale_factor <-
      scale[[paste0("scale_", scale_unit)]]
  } else {
    stopifnot(
      is.numeric(scale_factor),
      !is.null(actual_unit)
    )
  }

  dist <- convert_dist_units(
    dist = dist * scale_factor,
    # FIXME: This is a bit awkward in order to hijack
    to = actual_unit
  )

  if (!is.null(paper)) {
    paper$actual_width <- dist[[1]]
    paper$actual_height <- dist[[2]]
    paper$scale <- scale$scale
    return(paper)
  } else {
    return(dist)
  }
}
