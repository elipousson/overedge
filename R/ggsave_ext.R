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
#' @example examples/ggsave_ext.R
#' @seealso
#'  [ggplot2::ggsave()]
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
                       width = NULL,
                       height = NULL,
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
    if (is.null(filetype) && !is.null(filename) && stringr::str_detect(filename, "\\.")) {
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

#' @rdname ggsave_ext
#' @name ggsave_social
#' @inheritParams get_social_image
#' @export
#' @importFrom ggplot2 last_plot
#' @importFrom rlang list2 exec
ggsave_social <- function(plot = ggplot2::last_plot(),
                          image = "Instagram post",
                          platform = NULL,
                          format = NULL,
                          orientation = NULL,
                          name = NULL,
                          filename = NULL,
                          filetype = "jpeg",
                          dpi = 72,
                          width = 1080,
                          height = 1080,
                          units = "px",
                          ...) {
  image_size <-
    get_social_image(
      image = image,
      platform = platform,
      format = format,
      orientation = orientation
    )

  params <-
    modify_fn_fmls(
      params = rlang::list2(...),
      fn = ggsave_ext,
      plot = plot,
      width = image_size$width,
      height = image_size$height,
      name = name,
      filename = filename,
      filetype = filetype,
      units = units
    )

  rlang::exec(
    ggsave_ext,
    !!!params
  )
}

#' Get social media image size to match platform and format
#'
#' See `paper_sizes[paper_sizes$type == "social",]$name` for support image
#' options.
#'
#' @param image Image size name, Default: `NULL`
#' @param platform Social media platform, "Instagram", "Facebook", or "Twitter",
#'   Default: `NULL`
#' @param format Image format, "post", "story", or "cover", Default: `NULL`
#' @param orientation Image orientation, Default: `NULL`.
#' @rdname get_social_image
#' @export
#' @importFrom glue glue
get_social_image <- function(image = NULL, platform = NULL, format = NULL, orientation = NULL) {
  social_image_sizes <-
    paper_sizes[paper_sizes$type == "social", ]$name

  if (!is.null(image)) {
    image <- match.arg(image, social_image_sizes)
  } else {
    platform <- match.arg(platform, c("Instagram", "Twitter", "Facebook"))
    format <- match.arg(format, c("post", "story", "cover"))
    platform_image_sizes <- grep(glue::glue("^{platform}.+{format}"), social_image_sizes, value = TRUE)
    image <- match.arg(image, platform_image_sizes)
  }

  return(
    get_paper(
      paper = image,
      orientation = orientation
    )
  )
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
  is_pkg_installed("gt")

  if (is.null(filetype) && !is.null(filename) && stringr::str_detect(filename, "\\.")) {
    filetype <- stringr::str_extract(filename, "(?<=\\.).+$")
    filename <- str_remove_filetype(filename, filetype)
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
