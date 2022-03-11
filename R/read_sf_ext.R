#' Read spatial data in a bounding box to a simple feature object
#'
#' An extended version of \code{\link[sf]{read_sf}} that support reading spatial
#' data based on a file path, url (for spatial data or ArcGIS FeatureServer or
#' MapServer, or a Google Sheet with coordinate columns), or the data name and
#' associated package. Optionally provide a bounding box to filter data (not
#' supported for all data types).
#'
#' @param path file path; used by read_sf_path only
#' @param url url for spatial data file or for ArcGIS FeatureServer or MapServer
#'   to access with [get_esri_data()]; used by read_sf_url only
#' @param data character; name of data; used by read_sf_package only
#' @param package character; package name; used by read_sf_package only
#' @param filetype file type supported by \code{\link[sf]{read_sf}}., Default:
#'   'gpkg'; used by read_sf_package only and required only if the data is in
#'   the package cache directory or extdata system files.
#' @param bbox bbox object; Default: NULL. If bbox is provided, read_sf only
#'   returns features intersecting the bounding box.
#' @param coords Character vector with coordinate values; used for read_sf_url
#'   if the url is a Google Sheet
#' @param ... additional parameters passed to \code{\link[sf]{read_sf}}. May
#'   include query parameter.
#' @rdname read_sf_ext
#' @details read_sf_package looks for three types of package data:
#'   = Data loaded with the package
#'   - External data in the `extdata` system files folder.
#'   - Cached data in the cache directory returned by \code{\link[rappdirs]{user_cache_dir}}
#' @family read_write
#' @export
#' @importFrom checkmate check_file_exists
#' @importFrom sf read_sf
read_sf_path <- function(path, bbox = NULL, ...) {
  checkmate::check_file_exists(path)

  if (!is.null(bbox)) {
    # Convert bbox to well known text
    wkt <- sf_bbox_to_wkt(bbox = bbox)
    # Read external, cached, or data at path with wkt_filter
    data <- sf::read_sf(
      dsn = path,
      wkt_filter = wkt,
      ...
    )
  } else {
    data <- sf::read_sf(
      dsn = path,
      ...
    )
  }

  return(data)
}

#' @rdname read_sf_ext
#' @aliases read_sf_url
#' @export
#' @importFrom sf read_sf
read_sf_url <- function(url, bbox = NULL, coords = NULL, ...) {

  # Check url
  check_url(url)

  # Check MapServer or FeatureServer url
  if (check_esri_url(url)) {
    data <- get_esri_data(
      location = bbox,
      url = url,
      ...
    )
  } else if (check_gsheet_url(url)) {
    data <- googlesheets4::read_sheet(ss = url, ...)

    coords <- check_coords(x = data, coords = coords)

    data <- df_to_sf(data, coords = coords)
  } else {
    # TODO: Check if it is possible to use a WKT filter
    # when reading data from a url (e.g. a hosted GeoJSON file)
    data <- sf::read_sf(
      dsn = url,
      ...
    )
  }

  return(data)
}

#' @rdname read_sf_ext
#' @aliases read_sf_package
#' @export
#' @md
#' @importFrom checkmate test_directory_exists
#' @importFrom rappdirs user_cache_dir
#' @importFrom usethis ui_warn ui_yeah
#' @importFrom utils install.packages
read_sf_package <- function(data, bbox = NULL, package, filetype = "gpkg", ...) {
  check_package_exists(package)

  # Read package data
  package_files <- data(package = package)$results[, "Item"]

  if (data %in% package_files) {
    return(eval_data(data = data, package = package))
  }

  filename <- paste0(data, ".", filetype)
  extdata_files <- list.files(system.file("extdata", package = package))
  user_cache_dir_files <- list.files(rappdirs::user_cache_dir(package))

  # Get path to external package data or data from the package user cache
  if (filename %in% extdata_files) {
    # If data is in extdata folder
    path <- system.file("extdata", filename, package = package)
  } else if (filename %in% user_cache_dir_files) {
    # If data is in the cache directory
    path <- file.path(rappdirs::user_cache_dir(package), filename)
  }

  # Read data from path
  data <- read_sf_path(path = path, bbox = bbox, ...)

  return(data)
}


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
#' @importFrom purrr map_dfr
#' @importFrom fs dir_ls
#' @importFrom exiftoolr exif_read
#' @importFrom dplyr rename_with rename mutate case_when arrange
#' @importFrom janitor clean_names
#' @importFrom sf st_crs
read_sf_exif <- function(path = NULL,
                         bbox = NULL,
                         filetype = "jpg",
                         sort = "lon",
                         ...) {
  check_package_exists("exiftoolr")
  checkmate::check_directory_exists(path)

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

  # FIXME: Could filetype be inferred from the files at the path?
  data <-
    suppressMessages(
      purrr::map_dfr(
        fs::dir_ls(
          path = path,
          glob = paste0("*.", filetype)
        ),
        ~ exiftoolr::exif_read(
          .x,
          tags = tags
            # FIXME: The default fields likely vary by file type and could be set based on that
            # NOTE: Are there other tags that should be included by default?
        )
      )
    )

  data <-
    # Rename variables
    dplyr::rename_with(
      janitor::clean_names(data, "snake"),
      ~ sub("^gps_", "", .x)
    )

  data <-
    dplyr::rename(
      data,
      lon = longitude,
      lon_ref = longitude_ref,
      lat = latitude,
      lat_ref = latitude_ref,
      image_direction = img_direction,
      image_direction_ref = img_direction_ref,
      path = source_file,
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

  data <-
    dplyr::arrange(data, {{ sort }})

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
