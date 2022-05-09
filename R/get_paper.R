
#' Get standard paper and image sizes
#'
#' Use the "paper" parameter (matching name from [paper_sizes]), standard
#' (optionally including series and size) parameter, or width, height and units.
#' May return multiple paper sizes depending on parameters.
#'
#' If margin is provided, a block_width, block_height, and block_asp are
#' calculated and included as columns in the returned data frame.
#'
#' Paper can also be a data frame with "width", "height", "orientation", and
#' "units" columns.
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
#' @param width,height Width and height in units, Default: `NULL`.
#' @param units Paper size units, either "in", "mm", or "px"; defaults to `NULL`
#'   (using "in" if width or height are provided).
#' @param cols,rows Number of expected columns and rows in paper; used to
#'   determine row_height and section_asp in paper data frame returned by
#'   get_paper if rows or cols is greater than 1; defaults to `NULL`.
#' @param gutter Gutter distance in units. Gutter is used as the spacing between
#'   rows and columns (variable spacing is not currently supported); defaults to 0.
#' @inheritParams get_margin
#' @param bbox A bounding box to use to get orientation using [sf_bbox_asp()] with orientation = TRUE.
#' @param ... Additional parameters passed to get_margin. plot_width can only be
#'   passed in these parameters if paper has only a single row. margin is returned as a list column.
#' @return Data frame with one or more paper/image sizes.
#' @example examples/get_paper.R
#' @rdname get_paper
#' @export
#' @importFrom dplyr filter select mutate
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
                      rows = 1,
                      gutter = 0,
                      bbox = NULL,
                      margin = NULL,
                      ...) {
  if (!is.null(bbox)) {
    orientation <-
      sf_bbox_asp(bbox = bbox, orientation = TRUE)
  } else {
    orientation <-
      match.arg(
        orientation,
        c("portrait", "landscape", "square"),
        several.ok = TRUE
      )
  }

  if (is.data.frame(paper)) {
    is_df_paper(paper, ext = FALSE)

    stopifnot(
      nrow(paper) >= 1
    )
  } else if (is.character(paper)) {
    paper <- get_paper_name(paper)
  } else if (!is.null(standard)) {
    paper <- get_paper_standard(standard = standard, series = series, size = size)
  } else {
    paper <- get_paper_dims(width = width, height = height, units = units)
  }

  paper_orientation <- unique(paper$orientation)

  # Save width and height before checking orientation
  if (!is.null(orientation) && (length(paper_orientation) == 1)) {
    paper_width <- paper$width
    paper_height <- paper$height

    if ((paper_orientation == "portrait") && (orientation == "landscape")) {
      # width and height for most papers are assumed to be in a portrait format
      paper$width <- paper_height
      paper$height <- paper_width
      paper$orientation <- orientation
    } else if ((paper_orientation == "landscape") && (orientation == "portrait")) {
      # width and height for most papers are assumed to be in a portrait format
      paper$width <- paper_height
      paper$height <- paper_width
      paper$orientation <- orientation
    }
  }

  paper <-
    dplyr::mutate(
      paper,
      asp = width / height,
      .after = height
    )

  stopifnot(
    is.numeric(cols) && is.numeric(rows) && is.numeric(gutter)
  )

  paper <-
    dplyr::mutate(
      paper,
      cols = cols,
      col_width = (width - gutter) / cols,
      rows = rows,
      row_height = (height - gutter) / rows,
      gutter = gutter,
      section_asp = col_width / row_height,
      .after = asp
    )

  if (!is.null(margin)) {
    paper <- get_margin(paper = paper, margin = margin, ...)
  }

  return(paper)
}

#' Get paper using name
#'
#' @noRd
#' @importFrom dplyr filter
get_paper_name <- function(paper) {
  paper <-
    dplyr::filter(
      overedge::paper_sizes,
      tolower(.data$name) %in% tolower(paper)
    )
}

#' Get paper using standard and optional series or size
#'
#' @noRd
#' @importFrom dplyr filter
get_paper_standard <- function(standard, series = NULL, size = NULL) {
  paper_standard <- match.arg(standard, c("ANSI", "ISO", "British Imperial", "JIS", "USPS", "Facebook", "Instagram", "Twitter"), several.ok = TRUE)
  paper <- dplyr::filter(
    overedge::paper_sizes,
    .data$standard %in% paper_standard
  )

  if (!is.null(series)) {
    paper_series <- match.arg(series, c("A", "B", "C", "Engineering", "Architecture", "EDDM"), several.ok = TRUE)

    paper <- dplyr::filter(
      paper,
      .data$series %in% paper_series
    )
  }


  if (!is.null(size)) {
    paper_size <- size

    paper <- dplyr::filter(
      paper,
      .data$size %in% paper_size
    )
  }

  return(paper)
}

#' Get paper using width, height, or both
#'
#' @noRd
#' @importFrom dplyr filter
get_paper_dims <- function(width = NULL, height = NULL, units = NULL) {
  units <- match.arg(units, c("in", "mm", "px"))
  paper_units <- units

  if (!is.null(width) && !is.null(height)) {
    paper_width <- width
    paper_height <- height

    paper <- dplyr::filter(
      overedge::paper_sizes,
      .data$width %in% paper_width,
      .data$height %in% paper_height,
      .data$units %in% paper_units
    )
  } else if (!is.null(width)) {
    paper_width <- width

    paper <- dplyr::filter(
      overedge::paper_sizes,
      .data$width %in% paper_width,
      .data$units %in% paper_units
    )
  } else if (!is.null(height)) {
    paper_height <- height

    paper <- dplyr::filter(
      overedge::paper_sizes,
      .data$height %in% paper_height,
      .data$units %in% paper_units
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
#'   line length is planned but not yet implemented. Default: `NULL` (equivalent
#'   to "none").
#' @param dist Margin distance (single value used to all sides), Default: `NULL`
#' @param unit Unit for margin distance, Default: 'in'.
#' @param plot_width Plot or map width in units. If `paper` and `plot_width` are
#'   provided, margins are half the distance between the two evenly distributed.
#'   This sets the margin distance for height as well as width so does not work
#'   well with header and footers and should be improved in the future.
#' @param header,footer Header and footer height in units; defaults to 0. Please
#'   note: headers and footers are not currently supported for "px" units.
#' @inheritParams get_paper
#' @return A [ggplot2::margin()] element intended for use with
#'   [ggplot2::element_rect()] and the `plot.background` theme element.
#' @example examples/get_margin.R
#' @seealso
#'  [ggplot2::margin()]
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
  if (is.character(margin) || is.null(margin)) {
    margin <- match.arg(margin, c("none", "narrow", "standard", "extrawide", "wide"))
  }

  unit <- match.arg(unit, c("in", "mm", "px", "cm", "npc", "picas", "pc", "pt", "lines", "char", "native"))

  paper_is_df <- !is.null(paper) && is.data.frame(paper)

  if (!is.null(paper) && is.character(paper)) {
    paper <- get_paper(paper = paper, orientation = orientation)

    if (!is.null(plot_width)) {
      dist <- (paper$width - plot_width) / 2
    }
  } else if (paper_is_df) {
    is_df_paper(paper, ext = TRUE)

    # FIXME: get_paper only passes to get_margin if margin is a character value but the value of margin does not matter to set the dist if plot_width is provided.
    if (!is.null(plot_width) && (nrow(paper) == 1)) {
      dist <- (paper$width - plot_width) / 2
    }
  }

  if (is.null(dist)) {
    if (is.character(margin) && (margin != "auto")) {
      margin <-
        get_margin_type(paper = paper, type = margin, unit = unit)
    } else if (margin == "auto") {
      # TODO: implement margin settings that respond to font family and size and/or paper
      # e.g. LaTeX default is 1.5 in margin w/ 12pt text, 1.75 in for 11pt, 1.875 for 10pt
      # See https://practicaltypography.com/page-margins.html for more information on linelength and margins
    }
  } else if (unit != "px") {
    margin <-
      get_margin_dist(dist = dist, unit = unit)
  }

  # FIXME: What is this doing? I think it is
  if (unit != "px") {
    margin <- margin + grid::unit(x = c(header, 0, 0, footer), unit = unit)
  }

  if (!paper_is_df) {
    return(margin)
  }

  return(add_margin_to_paper(paper = paper, margin = margin))
}

#' Get margin based on distance
#'
#' @noRd
#' @importFrom ggplot2 margin
get_margin_dist <- function(dist = NULL, unit = "in") {
  if (length(dist) == 1) {
    margin <- ggplot2::margin(t = dist, r = dist, b = dist, l = dist, unit = unit)
  } else if (length(dist) == 4) {
    margin <- ggplot2::margin(t = dist[[1]], r = dist[[2]], b = dist[[3]], l = dist[[4]], unit = unit)
  }

  return(margin)
}

#' Get margin based on standard type
#'
#' @noRd
#' @importFrom ggplot2 margin
get_margin_type <- function(paper = NULL, type = "none", unit = "in") {
  if (unit == "in") {
    margin <- switch(type,
      "extrawide" = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = unit),
      "wide" = ggplot2::margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = unit),
      "standard" = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = unit),
      "narrow" = ggplot2::margin(t = 0.75, r = 0.75, b = 0.75, l = 0.75, unit = unit),
      "none" = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = unit)
    )
  } else if (unit == "mm") {
    margin <- switch(type,
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

    margin <- switch(type,
      "extrawide" = ggplot2::margin(px_to_npc_margins(120), unit = "npc"), # 1080 / 6
      "wide" = ggplot2::margin(px_to_npc_margins(80), unit = "npc"), # 1080 / 8
      "standard" = ggplot2::margin(px_to_npc_margins(40), unit = "npc"), # 1080 / 12
      "narrow" = ggplot2::margin(px_to_npc_margins(20), unit = "npc"),
      "none" = ggplot2::margin(px_to_npc_margins(0), unit = "npc")
    )
  }

  return(margin)
}

#' Get paper with margin
#'
#' @noRd
#' @importFrom dplyr mutate
add_margin_to_paper <- function(paper = NULL, margin = NULL) {
  # FIXME: This is mainly for the case of get_paper calling get_margin. It could be moved to get_paper or to a separate utility function.
  paper_margin <- margin
  margin_num <- as.numeric(paper_margin)
  margin_width <- margin_num[[2]] + margin_num[[4]]
  margin_height <- margin_num[[1]] + margin_num[[3]]

  paper <-
    dplyr::mutate(
      paper,
      block_width = width - margin_width,
      block_height = height - margin_height,
      block_asp = block_width / block_height,
      col_width = (block_width - gutter) / cols,
      row_height = (block_height - gutter) / rows,
      section_asp = col_width / row_height,
      margin = list(paper_margin)
    )

  return(paper)
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
#' @example examples/get_asp.R
#' @rdname get_asp
#' @export
get_asp <- function(asp = NULL,
                    paper = NULL,
                    orientation = NULL,
                    margin = NULL,
                    unit = NULL,
                    block_asp = FALSE) {
  if (is.null(paper)) {
    # Check aspect ratio
    asp <- get_char_asp(asp = asp)
  } else if (!is.null(paper) && is.null(asp)) {
    # Get aspect ratio for text/plot/map block area
    paper <- get_paper(paper = paper, orientation = orientation)

    if (block_asp) {
      asp <- get_block_asp(paper = paper, margin = margin, unit = unit)
    } else {
      asp <- paper$asp
    }
  }

  return(asp)
}

#' @noRd
#' @importFrom stringr str_extract
#' @importFrom cli cli_abort
get_char_asp <- function(asp) {
  if (is.character(asp) && grepl(":", asp)) {
    # If asp is provided as character string (e.g. "16:9") convert to a numeric
    # ratio
    asp <-
      as.numeric(stringr::str_extract(asp, ".+(?=:)")) /
        as.numeric(stringr::str_extract(asp, "(?<=:).+"))
  } else if (!is.numeric(asp) && !is.null(asp)) {
    cli::cli_abort("{.field {'asp'}} must be numeric (e.g. {.val {0.666}}) or a string with a width to height ratio (e.g. {.val {'4:6'}}).")
  }

  return(asp)
}

#' @noRd
#' @importFrom cli cli_abort
get_block_asp <- function(paper, orientation = NULL, margin = NULL, unit = NULL) {
  stopifnot(
    is.data.frame(paper)
  )

  if (is.null(unit)) {
    unit <- paper$units
  }

  # Get margins and convert to numeric (note substitute original value of paper for paper$name)
  if (is.character(margin)) {
    margin <- get_margin(margin = margin, paper = paper$name, orientation = orientation, unit = unit)
  } else if (is.numeric(margin)) {
    margin <- get_margin(dist = margin, unit = unit)
  } else if (!is_class(margin, classes = "margin")) {
    cli::cli_abort("{.field {'margin'}} must be either a character string matching the margin options ('none', 'narrow', 'standard', 'wide', or 'extrawide'),
                         a numeric vector that can be passed to the dist parameter of get_margins,
                         or a margin class object.")
  }

  margin <- suppressWarnings(as.numeric(margin))

  # Calculate width, height, and aspect ratio for text/plot/map block area
  block_width <- paper$width - (margin[[2]] + margin[[4]])
  block_height <- paper$height - (margin[[1]] + margin[[3]])
  asp <- block_width / block_height

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
