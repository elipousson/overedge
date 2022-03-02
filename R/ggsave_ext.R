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
#' @importFrom ggplot2 ggsave last_plot
#' @importFrom glue glue
#' @importFrom exifr exiftool_call
ggsave_ext <- function(plot = ggplot2::last_plot(),
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
