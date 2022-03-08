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

    coords <- check_coords(data = data, coords = coords)

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
#'   outside the bounding box)
#' @param filetype file extension or file type; defaults to "jpg"
#' @param sort variable to sort by. Currently supports "lon" (default), "lat",
#'   or "filename"
#' @param crs Coordinate reference system to
#' @param ... Additional EXIF tags to pass to exiftoolr::exif_read
#' @family read_write
#' @export
#' @importFrom checkmate check_directory_exists
#' @importFrom purrr map_dfr
#' @importFrom fs dir_ls
#' @importFrom exifr read_exif
#' @importFrom dplyr rename arrange mutate row_number
read_sf_exif <- function(path = NULL,
                         bbox = NULL,
                         filetype = "jpg",
                         sort = "lon",
                         ...) {
  check_package_exists("exiftoolr")
  checkmate::check_directory_exists(path)

  tags <-
    c(
      "SourceFile",
      "GPSImgDirection",
      "GPSLatitude",
      "GPSLongitude",
      "ImageDescription",
      "ImageWidth",
      "ImageHeight",
      "Orientation"
    )

  # FIXME: Could filetype be inferred from the files at the path?
  data <-
    suppressMessages(
      purrr::map_dfr(
        fs::dir_ls(
          path = path,
          glob = paste0("*.", filetype)
        ),
        ~ exifr::read_exif(
          .x,
          tags = c(
            # FIXME: The default fields likely vary by filetype and could be set based on that
            # NOTE: Are there other tags that should be included by default?
            tags,
            ...
          )
        )
      )
    )

  data <- data |>
    janitor::clean_names("snake") |>
    # Rename variables
    dplyr::rename(
      lat = gps_latitude,
      lon = gps_longitude,
      img_dir = gps_img_direction,
      filename = source_file
    ) |>
    dplyr::arrange({{ sort }})

  exif_crs <- 4326
  data <- df_to_sf(data, crs = exif_crs)


  if (!is.null(bbox)) {
    data <-
      get_location_data(
        location = bbox,
        data = data,
        from_crs = exif_crs
      )
  }

  return(data)
}
