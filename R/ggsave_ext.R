#' Save a ggplot and update file EXIF metadata
#'
#' Save a plot or map then update the EXIF metadata for the title, author, and
#' create data. `ggsave_ext()` also supports creating a file name based on a
#' sentence case name with spaces (e.g. "Baltimore city map") and appending a
#' label (e.g. "baltcity") as a prefix to the output file name.
#'
#' @param name name of plot, used to create filename (if filename is not
#'   provided)
#' @param label label is appended to filename as a prefix; defaults to NULL
#' @param prefix If "date", the date from `Sys.Date()` is appended to filename
#'   as a prefix (before label). If "time", `Sys.time()` is appended.
#' @param paper paper matching name from `paper_sizes` (e.g. "letter"). Not case
#'   sensitive
#' @param orientation "portrait", "landscape", or "square"
#' @param exif If TRUE, edit exif metadata for exported file using the exifr
#'   package, Default: FALSE
#' @param title title of plot or map, added to exif metadata, Default: NULL
#' @param author author of plot or map, added to exif metadata,
#' @param args args passed to exiftoolr, If args is not NULL, title and author
#'   are ignored, Default: NULL
#' @param ... additional parameters passed to \code{\link[ggplot2]{ggsave}}
#' @inheritParams ggplot2::ggsave
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
#'  \code{\link[exifr]{exiftool_call}}
#' @rdname ggsave_ext
#' @export
#' @importFrom checkmate check_character
#' @importFrom glue glue
#' @importFrom ggplot2 ggsave
#' @importFrom usethis ui_stop
#' @importFrom janitor make_clean_names
#' @importFrom exifr exiftool_call
ggsave_ext <- function(name = NULL,
                       label = NULL,
                       plot = last_plot(),
                       filename = NULL,
                       path = NULL,
                       device = "png",
                       paper = NULL,
                       orientation = "portrait",
                       scale = 1,
                       width,
                       height,
                       units = "in",
                       dpi = 300,
                       bg = NULL,
                       exif = FALSE,
                       title = NULL,
                       author,
                       args = NULL,
                       prefix = "date",
                       ...) {
  if (!any(sapply(c(name, device), is.null))) {
    checkmate::check_character(name)
    filename <- glue::glue("{janitor::make_clean_names(name)}.{device}")
  } else if (is.null(filename)) {
    usethis::ui_stop("If filename is NULL, both a name and device must be provided.")
  }

  if (!is.null(paper)) {
    paper <- get_paper(paper = paper, orientation = orientation)
    width <- paper$width
    height <- paper$height
    units <- paper$units
  }

  if (prefix == "date") {
    prefix <- gsub("^x", "", janitor::make_clean_names(Sys.Date(), sep_out = "-"))
  } else if (prefix == "time") {
    prefix <- gsub("^x", "", janitor::make_clean_names(Sys.time(), sep_out = "-"))
  } else {
    prefix <- NULL
  }

  if (!is.null(label)) {
    label <- janitor::make_clean_names(label)
  }

  filename <- paste0(c(prefix, label, filename), collapse = "_")

  if (!is.null(path)) {
    checkmate::check_directory_exists(path)
    filename <- file.path(path, filename)
  }

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = device,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bg,
    ...
  )

  if (exif) {
    check_package_exists("exifr")

    if (!is.null(title) & is.null(args)) {
      title <- glue::glue(title)

      exifr::exiftool_call(
        args = glue::glue(
          "-Author='{author}' ",
          "-Title='{title}' ",
          # FIXME: CreateDate works for PDFs but not PNGs
          "-CreateDate='", format(Sys.time(), "%Y:%m:%d %H:%M:%S"), "' ",
          "-overwrite_original"
        ),
        fnames = filename
      )
    } else if (!is.null(args)) {
      exifr::exiftool_call(
        args = args,
        fnames = filename
      )
    }
  }
}


#' Get paper sizes
#'
#' Use the "paper" parameter (matching name from `paper_sizes`), standard
#' (optionally including both series and size) parameter, or width, height and
#' units. May return multiple paper sizes.
#'
#' @param paper Paper, Default: 'letter'
#' @param orientation Orientation "portrait", "landscape", or "square", Default: 'portrait'
#' @param standard Size standard, "ANSI" or "ISO"
#' @param series Size series (e.g. A), Default: NULL
#' @param width width in units, Default: NULL
#' @param height height in units, Default: NULL
#' @param units "in" or "mm"; Default: NULL (defaults to "in" if width or height are provided.)
#' @return Data frame with paper sizes
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
#' @importFrom checkmate check_number
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
      tolower(name) == tolower(paper)
    )
  } else if (!is.null(standard)) {
    paper_standard <- match.arg(standard, c("ANSI", "ISO", "British Imperial", "JIS"))
    paper <- dplyr::filter(
      paper_sizes,
      standard == paper_standard
    )

    if (!is.null(size)) {
      paper_series <- match.arg(series, c("A", "B", "C", "Engineering", "Architecture"))
      paper_size <- size

      paper <- dplyr::filter(
        paper,
        size == paper_size,
        series == paper_series
      )
    }
  } else if (!is.null(width)) {
    paper_width <- width
    paper_height <- height

    units <- match.arg(units, c("in", "mm"))
    paper_units <- units

    paper <- dplyr::filter(
      paper_sizes,
      width == paper_width,
      height == paper_height,
      units == paper_units
    )
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
    checkmate::check_number(paper$asp_portrait, lower = 1, upper = 1)
  }

  return(paper)
}

#' Get margins for a ggplot2 plot or map based on style or distance
#'
#' This function works in combination with the `get_paper()` function to make it
#' easier to position a map on a page before saving to file. This is primarily
#' useful when adapting a gglpot2 map or plot to a print document format that is
#' composed outside of R using a page layout application such as Adobe InDesign.
#'
#' @param margin Margin style (options include "extrawide", "wide", "standard",
#'   "narrow", "none"), Additional "auto" option to generate margin based on
#'   line length is planned but not yet implemented. Default: NULL (equivalent to "none")
#' @param dist Margin distance (single value used to all sides), Default: NULL
#' @param unit Unit for margin distance, Default: 'in'
#' @param plot_width Plot or map width in units. If `paper` and `plot_width` are
#'   provided, margins are half the distance between the two evenly distributed.
#'   This is not tested and may not work with all page sizes/orientations.
#' @inheritParams get_paper
#' @return A ggplot2::margin() element intended for use with
#'   ggplot2::element_rect() and the plot.background theme element.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   get_margins("standard")
#'
#'   get_margins("none")
#'
#'   get_margins(dist = 25, unit = "mm")
#'
#'   get_margins(paper = "letter", plot_width = 5.5)
#' }
#' }
#' @seealso
#'  \code{\link[ggplot2]{margin}}
#' @rdname get_margin
#' @export
#' @importFrom ggplot2 margin
get_margin <- function(margin = NULL,
                       paper = NULL,
                       orientation = NULL,
                       dist = NULL,
                       unit = "in",
                       plot_width = NULL) {

  margin <- match.arg(margin, c("none", "narrow", "standard", "extrawide", "wide"))
  unit <- match.arg(unit, c("mm", "in"))

  if (!is.null(paper) && !is.null(plot_width)) {
    paper <- get_paper(paper = paper, orientation = orientation)
    dist <- (paper$width - plot_width) / 2
  }

  if (is.null(dist)) {
    if (is.character(margin) && (margin != "auto")) {
      if (unit == "in") {
        margin <- switch(margin,
          "extrawide" = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "in"),
          "wide" = ggplot2::margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = "in"),
          "standard" = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = "in"),
          "narrow" = ggplot2::margin(t = 0.75, r = 0.75, b = 0.75, l = 0.75, unit = "in"),
          "none" = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "in")
        )
      } else if (unit == "mm") {
        margin <- switch(margin,
          "extrawide" = ggplot2::margin(t = 80, r = 80, b = 80, l = 80, unit = unit),
          "wide" = ggplot2::margin(t = 60, r = 60, b = 60, l = 60, unit = unit),
          "standard" = ggplot2::margin(t = 40, r = 40, b = 40, l = 40, unit = unit),
          "narrow" = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = unit),
          "none" = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = unit)
        )
      }
    } else if (margin == "auto") {
      # TODO: implement margin settings that respond to font family and size and/or paper
      # e.g. LaTeX default is 1.5 in margin w/ 12pt text, 1.75 in for 11pt, 1.875 for 10pt
      # See https://practicaltypography.com/page-margins.html for more information on linelength and margins
    }
  } else {
    if (length(dist) == 1) {
      margin <- ggplot2::margin(t = dist, r = dist, b = dist, l = dist, unit = unit)
    } else if (length(dist) == 4) {
      margin <- ggplot2::margin(t = dist[[1]], r = dist[[2]], b = dist[[3]], l = dist[[4]], unit = unit)
    }
  }

  return(margin)
}
