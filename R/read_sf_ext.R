#' Read spatial data in a bounding box to a simple feature object
#'
#' An extended version of [sf::read_sf()] that support reading spatial
#' data based on a file path, url (for spatial data or ArcGIS FeatureServer or
#' MapServer, or a Google Sheet with coordinate columns), or the data name and
#' associated package. Optionally provide a bounding box to filter data (not
#' supported for all data types).
#'
#' @details Reading data from a package with `read_sf_pkg`:
#'
#' [read_sf_pkg()] looks for three types of package data:
#'
#'   = Data loaded with the package
#'   - External data in the `extdata` system files folder.
#'   - Cached data in the cache directory returned by [rappdirs::user_cache_dir]
#'
#' @param path A file path; used by [read_sf_path()] only
#' @param url A url for a spatial data file or for ArcGIS FeatureServer or
#'   MapServer to access with [get_esri_data()]; used by [read_sf_url()] only
#' @param data character; name of data; used by [read_sf_pkg()] only
#' @param package character; package name; used by [read_sf_pkg()] only
#' @param filetype file type supported by [sf::read_sf()]., Default: 'gpkg';
#'   used by [read_sf_pkg()] only and required only if the data is in the
#'   package cache directory or extdata system files.
#' @param bbox A bounding box object; Default: `NULL`. If "bbox" is provided,
#'   read_sf only returns features intersecting the bounding box.
#' @param coords Character vector with coordinate values; used for
#'   [read_sf_url()] if the "url" is a Google Sheet
#' @param ... additional parameters passed to [sf::read_sf()]. May include query
#'   parameter.
#' @name read_sf_ext
#' @family read_write
NULL

#' @rdname read_sf_ext
#' @name read_sf_path
#' @export
#' @importFrom sf read_sf
#' @importFrom checkmate check_file_exists
read_sf_path <- function(path, bbox = NULL, ...) {
  checkmate::check_file_exists(path)

  params <- rlang::list2(...)

  if (!("query" %in% names(params))) {
    if (!is.null(params$name) && !is.null(params$name_col)) {
      if (is.null(params$table)) {
        params$table <-
          stringr::str_extract(
            basename(path),
            "[:graph:]+(?=\\.)"
          )
      }

      params$query <-
        glue::glue("select * from {params$table} where {params$name_col} = '{params$name}'")
    }
  }

  if (!is.null(bbox)) {
    # Convert bbox to well known text
    wkt <- sf_bbox_to_wkt(bbox = bbox)
    # Read external, cached, or data at path with wkt_filter

    if (!is.null(params$query)) {
      data <- sf::read_sf(
        dsn = path,
        wkt_filter = wkt,
        query = params$query
      )
    } else {
        data <- sf::read_sf(
          dsn = path,
          wkt_filter = wkt)
    }
  } else {
    if (!is.null(params$query)) {
      data <- sf::read_sf(
        dsn = path,
        query = params$query
      )
    } else {
      data <- sf::read_sf(
        dsn = path)
    }
  }

  return(data)
}

#' @rdname read_sf_ext
#' @name read_sf_url
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
#' @name read_sf_pkg
#' @export
#' @md
#' @importFrom stringr str_detect
#' @importFrom utils data
read_sf_pkg <- function(data, bbox = NULL, package, filetype = "gpkg", ...) {
  check_pkg_installed(package)

  # Read package data
  pkg_files <- utils::data(package = package)$results[, "Item"]

  if (data %in% pkg_files) {
    return(eval_data(data = data, package = package))
  }

  if (stringr::str_detect(data, "\\.")) {
    filename <- data
  } else {
    filename <- paste0(data, ".", filetype)
  }

  pkg_extdata_files <- list.files(system.file("extdata", package = package))
  pkg_cache_dir <- get_data_dir(package = package)
  pkg_cache_dir_files <- list.files(pkg_cache_dir)

  # Get path to external package data or data from the package user cache
  if (filename %in% pkg_extdata_files) {
    # If data is in extdata folder
    path <- system.file("extdata", filename, package = package)
  } else if (filename %in% pkg_cache_dir_files) {
    # If data is in the cache directory
    path <- file.path(pkg_cache_dir, filename)
  }

  # Read data from path
  data <- read_sf_path(path = path, bbox = bbox, ...)

  return(data)
}

#' @rdname read_sf_ext
#' @aliases read_sf_download
#' @inheritParams utils::download.file
#' @inheritParams get_data_dir
#' @inheritParams make_filename
#' @export
#' @importFrom sf st_crs
#' @importFrom utils download.file
read_sf_download <-
  function(url,
           bbox = NULL,
           filename = NULL,
           path = NULL,
           filetype = "geojson",
           prefix = "date",
           method = "auto",
           ...) {
    # TODO: Update read_sf_download to support unzipping downloads and reading files from folder
    path <- get_data_dir(path = path)

    destfile <-
      make_filename(
        prefix = prefix,
        filename = filename,
        path = path,
        filetype = filetype
      )

    utils::download.file(
      url = url,
      destfile = destfile,
      method = method,
      ...
    )

    data <- read_sf_path(path = destfile, bbox = bbox)

    return(data)
  }

#' Read simple features using any of the extended methods
#'
#' @noRd
read_sf_any <- function(bbox = NULL, ...) {
  params <- rlang::list2(...)

  # FIXME: Look at the updated version of layer_location_data for a preferred method to set read_fn
  if (!is.null(params$path)) {
    read_fn <- "read_sf_path"
  } else if (!is.null(params$url)) {
    read_fn <- "read_sf_url"
  } else if (!is.null(params$data)) {
    read_fn <- "read_sf_pkg"
  } else if (!is.null(params$path) && is.null(params$method)) {
    read_fn <- "read_sf_path"
  } else if (!is.null(params$method)) {
    read_fn <- "read_sf_download"
  }

  if (any(sapply(list(params$path, params$url, params$data), length) > 1)) {
    args <- utils::modifyList(
      params,
      formals(rlang::expr(!!read_fn))
    )

    rlang::exec(rlang::expr(!!read_fn), !!!args)
  }
}
