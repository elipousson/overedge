#' Read spatial data in a bounding box to a simple feature object
#'
#' An extended version of [sf::read_sf()] that support reading spatial
#' data based on a file path, url (for spatial data or ArcGIS FeatureServer or
#' MapServer, or a Google Sheet with coordinate columns), or the data name and
#' associated package. Optionally provide a bounding box to filter data (not
#' supported for all data types).
#'
#' @details Reading data from a package:
#'
#' [read_sf_pkg] looks for three types of package data:
#'
#'   - Data loaded with the package
#'   - External data in the `extdata` system files folder.
#'   - Cached data in the cache directory returned by [rappdirs::user_cache_dir]
#'
#' @details Additional ... parameters:
#'
#' [read_sf_pkg] and [read_sf_download] both pass additional parameters
#' to [read_sf_path] which supports query, name_col, name, and table. name and
#' name_col are ignored if a query parameter is provided. If table is not
#' provided, a expected layer name is created based on the file path.
#'
#' [read_sf_url] pass the where, name_col, and name for any ArcGIS FeatureServer or
#' MapServer url (passed to [get_esri_data]) or sheet if the url is for a Google
#' Sheet (passed to [googlesheets4::read_sheet]), or a query or wkt filter
#' parameter if the url is some other type (passed to [sf::read_sf]).
#'
#' [read_sf_ext] is a flexible function that only has bbox as a named parameters
#' and all other parameters in ... are passed to one of the other overedge
#' read_sf functions.
#'
#' @param bbox A bounding box object; Default: `NULL`. If "bbox" is provided,
#'   read_sf only returns features intersecting the bounding box.
#' @param path A file path; used by [read_sf_path()] only.
#' @param url A url for a spatial data file or for ArcGIS FeatureServer or
#'   MapServer to access with [get_esri_data()]; used by [read_sf_url()] only
#' @param data character; name of data; used by [read_sf_pkg()] only
#' @param package character; package name; used by [read_sf_pkg()] only
#' @param filetype file type supported by [sf::read_sf()]., Default: 'gpkg';
#'   used by [read_sf_pkg()] only and required only if the data is in the
#'   package cache directory or extdata system files.
#' @param coords Character vector with coordinate values; used for
#'   [read_sf_url()] if the "url" is a Google Sheet.
#' @param ... additional parameters passed to multiple functions; see details.
#' @name read_sf_ext
#' @family read_write
#' @export
#' @importFrom rlang list2 exec
#' @importFrom dplyr case_when
#' @importFrom cli cli_abort
read_sf_ext <- function(..., bbox = NULL) {
  params <- rlang::list2(...)

  read_sf_fn <-
    dplyr::case_when(
      !is.null(params$package) ~ "pkg",
      !is.null(params$ss) ~ "gsheet",
      !is.null(params$url) ~ "url",
      !is.null(params$filename) ~ "download",
      !is.null(params$path) ~ "path"
    )

  read_sf_fn <-
    switch(read_sf_fn,
      "path" = read_sf_path,
      "pkg" = read_sf_pkg,
      "url" = read_sf_url,
      "gsheet" = read_sf_gsheet,
      "download" = read_sf_download
    )

  if (!is.function(read_sf_fn)) {
    cli::cli_abort("The parameters provided did not match any overedge read_sf function.")
  }

  args <-
    modify_fn_fmls(
      params = params,
      fn = read_sf_fn,
      missing = TRUE
    )

  rlang::exec(read_sf_fn, !!!args)
}

#' @rdname read_sf_ext
#' @name read_sf_pkg
#' @export
#' @md
#' @importFrom utils data
#' @importFrom cli cli_abort
#' @importFrom dplyr case_when
read_sf_pkg <- function(data, bbox = NULL, package = NULL, filetype = "gpkg", ...) {
  stopifnot(
    !is.null(package) & is_pkg_installed(package)
  )

  # Read package data
  if (data %in% ls_pkg_data(package)) {
    return(use_eval_parse(data = data, package = package))
  }

  filename <- str_add_filetype(data, filetype = filetype)

  path <-
    dplyr::case_when(
      # If data is in extdata folder
      filename %in% ls_pkg_extdata(package) ~ system.file("extdata", filename, package = package),
      # If data is in the cache directory
      filename %in% ls_pkg_cache(package) ~ file.path(get_data_dir(package = package), filename)
    )

  # Read data from path
  data <- read_sf_path(path = path, bbox = bbox, ...)

  return(data)
}

#' @rdname read_sf_ext
#' @name read_sf_path
#' @export
#' @importFrom sf read_sf
#' @importFrom checkmate check_file_exists
read_sf_path <- function(path, bbox = NULL, ...) {
  stopifnot(
    fs::file_exists(path)
  )

  params <- rlang::list2(...)

  make_query <- !rlang::has_name(params, "query") && all(rlang::has_name(params, c("name", "name_col")))

  if (make_query) {
    if (!rlang::has_name(params, "table")) {
      params$table <-
        stringr::str_extract(
          basename(path),
          "[:graph:]+(?=\\.)"
        )
    }

    params$query <-
      glue::glue("select * from {params$table} where {params$name_col} = '{params$name}'")
  }

  if (!is.null(bbox)) {
    # Convert bbox to well known text
    wkt <- sf_bbox_to_wkt(bbox = bbox)
  } else {
    wkt <- character(0)
  }
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
      wkt_filter = wkt
    )
  }

  return(data)
}

#' @rdname read_sf_ext
#' @name read_sf_url
#' @export
#' @importFrom sf read_sf
read_sf_url <- function(url, bbox = NULL, coords = NULL, ...) {
  params <- rlang::list2(...)

  stopifnot(
    # Check url
    is_url(url)
  )

  # Check MapServer or FeatureServer url
  if (is_esri_url(url)) {
    data <-
      get_esri_data(
        location = bbox,
        url = url,
        name_col = params$name_col,
        name = params$name,
        where = params$where
      )
  } else if (is_gsheet(url)) {
    data <-
      read_sf_gsheet(ss = url, coords = coords, sheet = params$sheet)
  } else {

    # FIXME: This is an awkward way to reset back to defaults
    if (is.null(params$query)) {
      params$query <- NA
    }

    if (is.null(params$wkt_filter)) {
      params$wkt_filter <- character(0)
    }

    # TODO: Check if it is possible to use a WKT filter
    # when reading data from a url (e.g. a hosted GeoJSON file)
    data <- sf::read_sf(
      dsn = url,
      query = params$query,
      wkt_filter = params$wkt_filter
    )

    # FIXME: This should be documented and maybe should be the default but optional
    data <- sf::st_zm(data)
  }

  return(data)
}

#' Convert Google Sheets url to sf
#' @name read_sf_gsheet
#' @rdname read_sf_ext
#' @export
read_sf_gsheet <- function(ss, coords = c("lon", "lat"), ask = FALSE, ...) {
  is_pkg_installed("googlesheets4")

  if (ask && is.missing(ss)) {
    ss <-
      googlesheets4::gs4_find(cli_ask("What is the name of the Google Sheet to return?"))
  }

  data <- googlesheets4::read_sheet(ss = ss, ...)
  coords <- check_coords(x = data, coords = coords)
  data <- df_to_sf(data, coords = coords)

  return(data)
}

#' @rdname read_sf_ext
#' @aliases read_sf_download
#' @param unzip If `TRUE`, url must be a zip file that is downloaded, unzipped
#'   into a temporary directory, and then read to a file using the specified
#'   file type.
#' @inheritParams utils::download.file
#' @inheritParams get_data_dir
#' @inheritParams make_filename
#' @export
#' @importFrom sf st_crs
#' @importFrom utils download.file unzip
read_sf_download <-
  function(url,
           bbox = NULL,
           filename = NULL,
           path = NULL,
           filetype = "geojson",
           prefix = "date",
           method = "auto",
           unzip = FALSE,
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
      method = method
    )

    if (unzip) {
      zipdest <-
        make_filename(
          prefix = prefix,
          filename = filename,
          path = tempdir(),
          filetype = filetype
        )

      utils::unzip(
        zipfile = destfile,
        exdir = tempdir(),
        overwrite = TRUE
      )

      destfile <- zipdest
    }


    data <- read_sf_path(path = destfile, bbox = bbox, ...)

    return(data)
  }

#' Join data from a Google Sheet to a simple feature object
#'
#' @noRd
#' @importFrom dplyr left_join
#' @importFrom sf st_drop_geometry
join_sf_gsheet <- function(data, ss = NULL, sheet = 1, key = NULL, suffix = c("", "_gsheet")) {
  if (cli_yeah("Are you ready to sync from Google Sheets back to an sf object?")) {
    sheet_data <-
      sf::st_drop_geometry(
        read_sf_gsheet(
          ss = ss,
          sheet = sheet,
          ask = TRUE
        )
      )

    if (!is.null(key)) {
      data <-
        dplyr::left_join(
          sheet_data,
          data,
          by = key,
          suffix = suffix
        )
    }
  }

  return(data)
}
