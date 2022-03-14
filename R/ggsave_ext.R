#' Save a ggplot2 plot or gt table to file and update file EXIF metadata
#'
#' Save a plot or map then update the EXIF metadata for the title, author, and
#' create data. [ggsave_ext()] also supports creating a file name based on a
#' sentence case name with spaces (e.g. "Baltimore city map") and appending a
#' label (e.g. "baltcity") as a prefix to the output file name.
#'
#' @section gtsave_ext:
#'
#'   The gtsave_ext wraps the equivalent function [gt::gtsave()] with a similar
#'   structure for creating custom file names. This function does not currently
#'   support EXIF metadata updates although this option may be added in the future.
#'
#' @inheritParams ggplot2::ggsave
#' @param gt_object A gt table to save to file.
#' @param name Plot name, used to create filename (if filename is `NULL`) using
#'   [make_filename()]
#' @inheritParams make_filename
#' @param paper Paper matching name from `paper_sizes` (e.g. "letter"). Not case
#'   sensitive.
#' @param orientation Page orientation ("portrait", "landscape", or "square").
#' @param bgcolor Background color to optionally override `plot.background`
#'   theme element.
#' @param exif If `TRUE`, the EXIF metadata for the exported file is updated
#'   with the exifr package; defaults to `FALSE`.
#' @inheritParams write_exif
#' @param ... Additional parameters passed to [ggplot2::ggsave()] or
#'   [gt::gtsave()].
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

#' @name gtsave_ext
#' @rdname ggsave_ext
#' @export
gtsave_ext <- function(gt_object,
                       name = NULL,
                       label = NULL,
                       prefix = NULL,
                       postfix = NULL,
                       filename = NULL,
                       filetype = NULL,
                       path = NULL,
                       ...) {

  check_pkg_installed("gt")

  if (is.null(filetype) && stringr::str_detect(filename, "\\.")) {
    filetype <- stringr::str_extract(filename, "(?<=\\.).+$")
    filename <- stringr::str_remove(filename, paste0("\\.", filetype, "$"))
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

  gt::gtsave(
    data = gt_object,
    filename = filename,
    ...
  )

}
