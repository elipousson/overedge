
#' Read EXIF location data from images to a simple feature object
#'
#' Read EXIF data from folder of images.
#'
#' @param path path to folder of one or more files with EXIF location metadata
#' @param bbox bounding box to crop sf file (excluding images with location data
#'   outside the bounding box). If bbox is provided the returned data will match
#'   the crs of the bbox.
#' @param filetype file extension or file type; defaults to "jpg"
#' @param sort variable to sort by. Currently supports "lon" (default), "lat",
#'   or "filename"
#' @param ... Additional EXIF tags to pass to `exiftoolr::exif_read`
#' @family read_write
#' @export
#' @importFrom checkmate check_directory_exists
#' @importFrom usethis ui_warn ui_stop
#' @importFrom fs dir_ls
#' @importFrom stringr str_extract
#' @importFrom purrr map_dfr
#' @importFrom exiftoolr exif_read
#' @importFrom dplyr rename_with rename mutate case_when arrange
#' @importFrom janitor clean_names
#' @importFrom sf st_crs
read_sf_exif <- function(path = NULL,
                         filetype = NULL,
                         bbox = NULL,
                         sort = "lon",
                         tags = NULL,
                         ...) {
  check_pkg_installed("exiftoolr")
  checkmate::check_directory_exists(path)

  # FIXME: This is a partial list of filetypes that support GPS EXIF metadata
  # filetype <- match.arg(filetype, c("jpg", "jpeg", "png", "tiff", "pdf"))

  if (is.null(tags)) {
    # FIXME: The default fields likely vary by file type and could be set based on that
    # NOTE: Are there other tags that should be included by default?
    tags <-
      c(
        "ImageDescription",
        "FileName",
        "CreateDate",
        "DateTimeOriginal",
        "OffsetTimeOriginal",
        "ImageWidth",
        "ImageHeight",
        "Orientation",
        "SourceFile",
        "FileSize",
        "FileType",
        "*GPS*",
        ...
      )

    check_tags <- FALSE
    lonlat_tags <- TRUE
    lonlat_ext_tags <- TRUE
    orientation_tags <- TRUE
  } else if (!any(grepl("GPS", tags))) {
    usethis::ui_warn("The tags must include GPS values to create a simple feature object based on the file EXIF data.")
    check_tags <- TRUE
  }

  if (is.null(filetype)) {
    dir_files <- fs::dir_ls(path)
    filetype <- unique(stringr::str_extract(dir_files, "(?<=\\.).+$"))

    if (length(filetype) > 1) {
      usethis::ui_stop("The path {usethis::ui_path(path)} includes files with multiple types so a filetype parameter must be provided.")
    }
  }

  if (!is.null(filetype)) {
    glob <- paste0("*.", filetype)
  }

  data <-
    suppressMessages(
      purrr::map_dfr(
        fs::dir_ls(
          path = path,
          glob = glob
        ),
        ~ exiftoolr::exif_read(
          .x,
          tags = tags
        )
      )
    )

  data <-
    # Rename variables
    dplyr::rename_with(
      janitor::clean_names(data, "snake"),
      ~ sub("^gps_", "", .x)
    )

  if (check_tags) {
    lonlat_tags <- all(c("latitude", "longitude") %in% names(data))
    lonlat_ext_tags <- all(c("longitude_ref", "latitude_ref", "img_direction", "img_direction_ref", "source_file") %in% names(data))
    orientation_tags <- all(c("orientation", "image_width", "image_height") %in% names(data))
  }

  if (lonlat_tags) {
    data <-
      dplyr::rename(
        data,
        lon = longitude,
        lat = latitude
      )
  }

  if (lonlat_ext_tags) {
    data <-
      dplyr::rename(
        data,
        lon_ref = longitude_ref,
        lat_ref = latitude_ref,
        image_direction = img_direction,
        image_direction_ref = img_direction_ref,
        path = source_file
      )
  }

  if (orientation_tags) {
    data <-
      dplyr::rename(
        data,
        exif_orientation = orientation
      )

    data <-
      dplyr::mutate(
        data,
        exif_orientation =
          dplyr::case_when(
            exif_orientation == 1 ~ "Horizontal (normal)",
            exif_orientation == 2 ~ "Mirror horizontal",
            exif_orientation == 3 ~ "Rotate 180",
            exif_orientation == 4 ~ "Mirror vertical",
            exif_orientation == 5 ~ "Mirror horizontal and rotate 270 CW",
            exif_orientation == 6 ~ "Rotate 90 CW",
            exif_orientation == 7 ~ "Mirror horizontal and rotate 90 CW",
            exif_orientation == 8 ~ "Rotate 270 CW"
          ),
        orientation =
          dplyr::case_when(
            (image_width / image_height) > 1 ~ "landscape",
            (image_width / image_height) < 1 ~ "portrait",
            (image_width / image_height) == 1 ~ "square"
          )
      )
  }

  if (sort %in% names(data)) {
    data <-
      dplyr::arrange(data, .data[[sort]])
  } else {
    usethis::ui_warn("The provided value for sort {usethis::ui_value(sort)} is not found in the data.")
  }

  exif_crs <- 4326

  data <- df_to_sf(data, crs = exif_crs)

  if (!is.null(bbox)) {
    data <-
      get_location_data(
        location = bbox,
        data = data,
        from_crs = exif_crs,
        crs = sf::st_crs(bbox)
      )
  }

  return(data)
}


#' @name write_exif
#' @rdname read_sf_exif
#' @section Writing EXIF metadata
#' @param title Title to add to file metadata with exiftoolr, Default: `NULL`.
#' @param author Author to add to file metadata with exiftoolr, Default: `NULL`.
#' @param date Date to add to file metadata with exiftoolr (not currently
#'   working), Default: `NULL`.
#' @param keywords Keyword(s) added to file metadata with with exiftoolr,
#'   Default: `NULL`.
#' @param args Alternate arguments passed to [exiftoolr::exif_call()]. If args
#'   is not `NULL`, title, author, date, and keywords are ignored; defaults to
#'   `NULL`.
#' @param overwrite If TRUE, overwrite any existing EXIF metadata present in the
#'   provided fields; defaults to TRUE
#' @export
#' @importFrom glue glue
#' @importFrom exiftoolr exif_call
#' @importFrom usethis ui_done
write_exif <- function(path = NULL,
                       filetype = NULL,
                       title = NULL,
                       author = NULL,
                       date = NULL,
                       keywords = NULL,
                       args = NULL,
                       overwrite = TRUE) {
  check_pkg_installed("exiftoolr")

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
      # TODO: Add support for subjects (partially complete with keywords) https://stackoverflow.com/questions/28588696/python-exiftool-combining-subject-and-keyword-tags#28609886
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