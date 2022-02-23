#' Save a ggplot and update file EXIF metadata
#'
#' Save a plot or map then update the EXIF metadata for the title, author, and
#' create data. [ggsave_ext()] also supports creating a file name based on a
#' sentence case name with spaces (e.g. "Baltimore city map") and appending a
#' label (e.g. "baltcity") as a prefix to the output file name.
#'
#' @param name Plot name, used to create filename (if filename is `NULL`) using
#'   [make_filename()]
#' @inheritParams make_filename
#' @param title Title of plot or map, added to EXIF metadata, Default: `NULL`.
#' @param author Author of plot or map, added to EXIF metadata, Default: `NULL`.
#' @param paper Paper matching name from `paper_sizes` (e.g. "letter"). Not case
#'   sensitive.
#' @param orientation Page orientation ("portrait", "landscape", or "square").
#' @param bgcolor Background color to optionally override `plot.background`
#'   theme element.
#' @param exif If `TRUE`, the EXIF metadata for the exported file is updated
#'   with the exifr package; defaults to `FALSE`.
#' @param args Alternate arguments passed to \code{\link[exifr]{exiftool_call}}.
#'   If args is not `NULL`, title and author are ignored; defaults to `NULL`.
#' @param ... Additional parameters passed to \code{\link[ggplot2]{ggsave}}
#' @inheritParams ggplot2::ggsave
#' @inheritParams make_filename
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   neighborhoods <-
#'     get_location(
#'       type = "neighborhoods",
#'       package = "mapbaltimore"
#'     )
#'
#'   ggplot2::ggplot() +
#'     ggplot2::geom_sf(data = neighborhoods)
#'
#'   ggsave_ext(
#'     name = "neighborhoods",
#'     label = "Baltimore City",
#'     author = "Thomas H. Poppleton",
#'     device = "pdf",
#'     paper = "letter"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[ggplot2]{ggsave}}
#'  \code{\link[exifr]{exiftool_call}}
#' @rdname ggsave_ext
#' @export
#' @importFrom ggplot2 ggsave
#' @importFrom glue glue
#' @importFrom exifr exiftool_call
ggsave_ext <- function(plot = last_plot(),
                       name = NULL,
                       label = NULL,
                       prefix = NULL,
                       postfix = NULL,
                       filename = NULL,
                       device = "png",
                       filetype = NULL,
                       path = NULL,
                       paper = NULL,
                       orientation = "portrait",
                       width,
                       height,
                       units = "in",
                       scale = 1,
                       dpi = 300,
                       bgcolor = NULL,
                       exif = FALSE,
                       title = NULL,
                       author = NULL,
                       args = NULL,
                       ...) {
  if (!is.null(paper)) {
    paper <- get_paper(paper = paper, orientation = orientation)
    width <- paper$width
    height <- paper$height
    units <- paper$units
  }

  stopifnot(
    is.numeric(width) && is.numeric(height)
  )

  if (is.null(filetype)) {
    filetype <- device
  }

  filename <-
    make_filename(
      name = name,
      label = label,
      filename = filename,
      filetype = filetype,
      path = path,
      prefix = prefix,
      postfix = postfix
    )

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = device,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bgcolor,
    ...
  )

  if (exif) {
    check_package_exists("exifr")

    if (is.null(args)) {
      title <- glue::glue(title)

      if (device == "png") {
        create_date <-
          paste0("-CreationTime=now ")
      } else {
        create_date <-
          paste0(
            "-CreateDate=now ",
            "-ModifyDate=now "
          )
      }

      exifr::exiftool_call(
        args = glue::glue(
          "-Author='{author}' ",
          "-Title='{title}' ",
          "{create_date}",
          "-overwrite_original"
        ),
        fnames = filename,
        quiet = TRUE
      )
    } else if (!is.null(args)) {
      exifr::exiftool_call(
        args = args,
        fnames = filename
      )
    }
  }
}


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
get_paper <- function(paper = "letter",
                      orientation = "portrait",
                      standard = NULL,
                      series = NULL,
                      size = NULL,
                      width = NULL,
                      height = NULL,
                      units = NULL) {
  orientation <- match.arg(orientation, c("portrait", "landscape", "square"))

  if (!is.null(standard) | !is.null(width)) {
    paper <- NULL
  }

  if (!is.null(paper)) {
    paper <- dplyr::filter(
      paper_sizes,
      tolower(name) %in% tolower(paper)
    )
  } else if (!is.null(standard)) {
    paper_standard <- match.arg(standard, c("ANSI", "ISO", "British Imperial", "JIS", "USPS", "Facebook", "Instagram", "Twitter"))
    paper <- dplyr::filter(
      paper_sizes,
      standard %in% paper_standard
    )

    if (!is.null(series)) {
      paper_series <- match.arg(series, c("A", "B", "C", "Engineering", "Architecture", "EDDM"))

      paper <- dplyr::filter(
        paper,
        series %in% paper_series
      )

      if (!is.null(size)) {
        paper_size <- size

        paper <- dplyr::filter(
          paper,
          size %in% paper_size
        )
      }
    }
  } else if (!is.null(width)) {
    paper_width <- width

    units <- match.arg(units, c("in", "mm", "px"))
    paper_units <- units

    paper <- dplyr::filter(
      paper_sizes,
      width %in% paper_width,
      units %in% paper_units
    )

    if (!is.null(height)) {
      paper_height <- height

      paper <- dplyr::filter(
        paper_sizes,
        height %in% paper_height
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
