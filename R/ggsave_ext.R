#' Save a ggplot and update file EXIF metadata
#'
#' Save a plot or map then update the EXIF metadata for the title, author, and
#' create data. `ggsave_ext()` also supports creating a file name based on a
#' sentence case name with spaces (e.g. "Baltimore city map") and appending a
#' label (e.g. "baltcity") as a prefix to the output file name.
#'
#' @param name name of plot, used to create filename (if filename is not provided)
#' @param label label is appended to filename as a prefix.
#' @param prefix If "date", the date from `Sys.Date()` is appended to filename as a prefix (before label). If "time", `Sys.time()` is appended.
#' @param paper paper matching name from `paper_sizes` (e.g. "letter"). Not case sensitive
#' @param orientation "portrait", "landscape", or "square"
#' @param exif If TRUE, edit exif metadata for exported file using the exifr package, Default: TRUE
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
                       label,
                       plot = last_plot(),
                       filename = NULL,
                       path = NULL,
                       device,
                       paper = NULL,
                       orientation = "portrait",
                       scale = 1,
                       width,
                       height,
                       units = "in",
                       dpi = 300,
                       bg = NULL,
                       exif = TRUE,
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
    prefix <- gsub("^x","", janitor::make_clean_names(Sys.Date(), sep_out = "-"))
  } else if (prefix == "time") {
    prefix <- gsub("^x","", janitor::make_clean_names(Sys.time(), sep_out = "-"))
  } else {
    prefix <- NULL
  }

  label <- janitor::make_clean_names(label)

  filename <- paste0(c(prefix, label, filename), collapse = "_")

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = device,
    path = path,
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

    checkmate::check_directory_exists(path)

    fname <- file.path(path, filename)

    if (!is.null(title) & is.null(args)) {
      exifr::exiftool_call(
        args = glue::glue(
          "-Author='{author}' ",
          "-Title='{title}' ",
          # FIXME: CreateDate works for PDFs but not PNGs
          "-CreateDate='", format(Sys.time(), "%Y:%m:%d %H:%M:%S"), "' ",
          "-overwrite_original"
        ),
        fnames = fname
      )
    } else if (!is.null(args)) {
      exifr::exiftool_call(
        args = args,
        fnames = fname
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
    paper_sizes <-
      dplyr::rename(
        paper_sizes,
        asp = asp_portrait
      )
  } else if (orientation == "landscape") {
    paper$width <- height
    paper$height <- width

    paper_sizes <-
      dplyr::rename(
        paper_sizes,
        asp = asp_landscape
      )
  } else if (orientation == "square") {
    checkmate::check_number(paper$asp_portrait, lower = 1, upper = 1)
  }

  return(paper)
}
