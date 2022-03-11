#' Save a ggplot and update file EXIF metadata
#'
#' Save a plot or map then update the EXIF metadata for the title, author, and
#' create data. [ggsave_ext()] also supports creating a file name based on a
#' sentence case name with spaces (e.g. "Baltimore city map") and appending a
#' label (e.g. "baltcity") as a prefix to the output file name.
#'
#' @inheritParams ggplot2::ggsave
#' @param name Plot name, used to create filename (if filename is `NULL`) using
#'   [make_filename()]
#' @inheritParams make_filename
#' @param title Title of plot or map, added to file metadata with exiftoolr,
#'   Default: `NULL`.
#' @param author Author of plot or map, added to file metadata with exiftoolr,
#'   Default: `NULL`.
#' @param keywords Keyword(s) added to file metadata with with exiftoolr,
#'   Default: `NULL`.
#' @param paper Paper matching name from `paper_sizes` (e.g. "letter"). Not case
#'   sensitive.
#' @param orientation Page orientation ("portrait", "landscape", or "square").
#' @param bgcolor Background color to optionally override `plot.background`
#'   theme element.
#' @param exif If `TRUE`, the EXIF metadata for the exported file is updated
#'   with the exifr package; defaults to `FALSE`.
#' @param args Alternate arguments passed to [exiftoolr::exif_call()].
#'   If args is not `NULL`, title and author are ignored; defaults to `NULL`.
#' @param ... Additional parameters passed to [ggplot2::ggsave()]
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
#' @importFrom ggplot2 ggsave last_plot
#' @importFrom stringr str_detect str_extract str_remove
ggsave_ext <- function(plot = ggplot2::last_plot(),
                       name = NULL,
                       label = NULL,
                       prefix = NULL,
                       postfix = NULL,
                       filename = NULL,
                       device = NULL,
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
                       keywords = NULL,
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

  if (is.null(device)) {
    if (is.null(filetype) && stringr::str_detect(filename, "\\.")) {
      filetype <- stringr::str_extract(filename, "(?<=\\.).+$")
      filename <- stringr::str_remove(filename, paste0("\\.", filetype, "$"))
    }

    if (!is.null(filetype)) {
      device <- filetype
    }
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
    write_exif(path = filename, filetype = filetype, title = title, author = author, keywords = keywords, date = NULL, args = args)
  }
}

#' @name write_exif
#' @rdname ggsave_ext
#' @export
#' @importFrom glue glue
#' @importFrom exiftoolr exif_call
#' @importFrom usethis ui_done ui_path
write_exif <- function(path = NULL, filetype = NULL, title = NULL, author = NULL, date = NULL, keywords = NULL, args = NULL, overwrite = TRUE) {
  check_package_exists("exifr")
  # FIXME: I want to implement a method that allows adding, replacing, or modifying exif
  if (is.null(args)) {
    if (!is.null(title)) {
      args <- c(args, "-Title=Untitled")
    } else {
      args <- c(args, glue::glue("-Title={title}"))
    }

    if (!is.null(author)) {
      args <- c(args, glue::glue("-Author={author}"))
    }

    if (!is.null(date)) {
      # FIXME: exiftoolr::exif_call() does not support the "now" value supported by exif
      # If CreateDate is set to now automatically, why bother revising with exiftoolr anyway?
      # TODO: Add support for subjects https://stackoverflow.com/questions/28588696/python-exiftool-combining-subject-and-keyword-tags#28609886
      date <- "now"
      if ("png" %in% filetype) {
        args <- c(args, glue::glue("-CreationTime={date}"))
      } else {
        args <- c(args, c("-CreateDate={date}", "-ModifyDate={date}"))
      }
    }

    if (!is.null(keywords)) {
      args <- c(args, paste0("-Keywords+=", keywords))
    }

    if (overwrite) {
      args <- c(args, "-overwrite_original")
    }
  }

  if (!is.null(args)) {
    suppressMessages(
      suppressWarnings(
        exiftoolr::exif_call(
          args = args,
          path = path,
          quiet = TRUE
        )
      )
    )

    usethis::ui_done("EXIF metadata updated for {usethis::ui_path(path)}")
  }
}
