#' Save a ggplot and update file EXIF metadata
#'
#' Save a plot or map then update the EXIF metadata for the title, author, and
#' create data. `ggsave_ext()` also supports creating a file name based on a
#' sentence case name with spaces (e.g. "Baltimore city map") and appending a
#' label (e.g. "baltcity") as a prefix to the output file name.
#'
#' @param name name of plot, used to create filename
#' @param label label is appending to filename as a prefix.
#' @param asp aspect ratio
#' @param exif If TRUE, edit exif metadata for exported file, Default: TRUE
#' @param title title of plot or map, added to exif metadata, Default: NULL
#' @param author author of plot or map, added to exif metadata,
#' @param args args passed to exiftoolr, If args is not NULL, title and author
#'   are ignored, Default: NULL
#' @param ... additional parameters passed to \code{\link[ggplot2]{ggsave}}
#' @seealso
#'  \code{\link[exifr]{exiftool_call}}
#' @rdname ggsave_ext
#' @export
#' @importFrom checkmate check_character
#' @importFrom glue glue
#' @importFrom usethis ui_stop
#' @importFrom janitor make_clean_names
#' @importFrom exifr exiftool_call
ggsave_ext <- function(name = NULL,
                        label,
                        plot = last_plot(),
                        filename = NULL,
                        path = NULL,
                        device,
                        scale = 1,
                        asp,
                        width,
                        height,
                        units = "in",
                        dpi,
                        exif = TRUE,
                        title = NULL,
                        author,
                        args = NULL,
                        ...) {
  if (!any(sapply(c(name, device), is.null))) {
    checkmate::check_character(name)
    filename <- glue::glue("{janitor::make_clean_names(name)}.{device}")
  } else if (is.null(filename)) {
    usethis::ui_stop("If filename is NULL, both a name and device must be provided.")
  }

  filename <- paste0(c(janitor::make_clean_names(label), filename), collapse = "_")

  ggsave(
    filename = filename,
    plot = plot,
    device = device,
    path = path,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    ...
  )

  if (exif) {
    check_package_exists("exifr")

    if (!is.null(title) & is.null(args)) {
      exifr::exiftool_call(
        args = glue::glue(
          "-Author='{author}' ",
          "-Title='{title}' ",
          # FIXME: CreateDate works for PDFs but not PNGs
          "-CreateDate='", format(Sys.time(), "%Y:%m:%d %H:%M:%S"), "' ",
          "-overwrite_original"
        ),
        fnames = file.path(path, filename)
      )
    } else if (!is.null(args)) {
      exifr::exiftool_call(
        args = args,
        fnames = file.path(path, filename)
      )
    }
  }
}
