#' Get data for a location
#'
#' Returns data for a selected area or areas with an optional buffer. If both
#' crop and trim are FALSE, the function uses \code{\link[sf]{st_intersects}} to
#' filter data to include the full geometry of anything that overlaps with the
#' area or bbox (if the area is not provided).
#'
#' @details Working with sf lists for data and locations:
#'
#'   map_data_location makes it easier to work with sf lists. It supports data
#'   as a character vector, data as an sf list when location is a single object,
#'   location as a character vector or sf list (including lists of bbox or sfc
#'   objects), or when both data and location are lists (such as a list created
#'   by make_location_data_list).
#'
#' @param location sf object. If multiple areas are provided, they are unioned
#'   into a single sf object using \code{\link[sf]{st_union}}
#' @inheritParams st_bbox_ext
#' @param data sf object including data in area. data may also be a url or file
#'   path. data can be the name of a data object or, if package and filetype are
#'   provided, a cached or external file.
#' @param url url for FeatureServer or MapServer layer to pass to
#'   get_area_esri_data. url can be provided to data parameter
#' @param path path to spatial data file supported by \code{\link[sf]{read_sf}}
#' @param package package name.
#' @param filetype file type supported by \code{\link[sf]{read_sf}}. The file
#'   type must be provided for extdata and cached data.
#' @param fn Function to apply to data before returning.
#' @param crop  If TRUE, data cropped to location or bounding box
#'   \code{\link[sf]{st_crop}} adjusted by the `dist`, `diag_ratio`, and `asp`
#'   parameters provided. Default TRUE.
#' @param trim  If TRUE, data trimmed to area with
#'   \code{\link[sf]{st_intersection}}. This option is not supported for any
#'   adjusted areas that use the `dist`, `diag_ratio`, or `asp` parameters.
#'   Default FALSE.
#' @param from_crs coordinate reference system of the data.
#' @param crs coordinate reference system to return
#' @param class Class of
#' @param ... additional parameters passed to read_sf_path, read_sf_url, or read_sf_pkg
#' @rdname get_location_data
#' @export
#' @importFrom usethis ui_yeah ui_warn
#' @importFrom checkmate test_class test_file_exists
#' @importFrom sf st_crs st_crop st_transform st_intersection st_filter
#' @importFrom rlang as_function
get_location_data <- function(location = NULL,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              data = NULL,
                              url = NULL,
                              path = NULL,
                              package = NULL,
                              filetype = "gpkg",
                              fn = NULL,
                              crop = TRUE,
                              trim = FALSE,
                              from_crs = NULL,
                              crs = NULL,
                              class = "sf",
                              label = NULL,
                              ...) {
  params <- rlang::list2(...)

  if (!is.null(location)) {
    # Get adjusted bounding box using any adjustment variables provided
    bbox <-
      st_bbox_ext(
        x = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = from_crs
      )
  } else {
    bbox <- NULL
  }

  if (is_bbox(data)) {
    data <- as_sf(data)
  }

  # Check if data is not an  sf object
  if (!(is_sf(data))) {

    # Check if data is a url
    if (check_url(data)) {
      url <- data
      # Check if data is a path to an existing file
    } else if (checkmate::test_file_exists(x = data)) {
      path <- data
    }

    # Call the appropriate read_sf function
    if (!is.null(path)) {
      data <- read_sf_path(path = path, bbox = bbox, ...)
    } else if (!is.null(url)) {
      data <- read_sf_url(url = url, bbox = bbox, ...)
    } else if (!is.null(package)) {
      data <- read_sf_pkg(data = data, bbox = bbox, package = package, filetype = filetype, ...)
    }
  }

  if (!is.null(bbox)) {
    if (trim) {
      # If trim, match location crs to data
      location <- st_transform_ext(x = location, crs = data)
      data <- st_erase(x = data, y = location, flip = TRUE)
    } else {
      # Otherwise, match bbox crs to data
      bbox <- st_transform_ext(x = bbox, crs = data)

      if (crop) {
        data <- suppressWarnings(sf::st_crop(data, bbox))
      } else {
        # If no cropping, filter with bbox
        bbox_sf <- sf_bbox_to_sf(bbox)
        data <- sf::st_filter(data, bbox_sf)
      }
    }
  }

  if (!is.null(fn)) {
    fn <- rlang::as_function(fn)
    data <- fn(data)
  }

  if (!is.null(params$col)) {
    col <- params$col
  } else {
    col <- NULL
  }

  data <- as_sf_class(x = data, class = class, crs = crs, col = col)

  return(data)
}

#' @name map_location_data
#' @rdname get_location_data
#' @param load If TRUE and class is "list", load data to local environment; defaults FALSE.
#' @export
#' @importFrom rlang list2
#' @importFrom janitor make_clean_names
#' @importFrom purrr set_names map_chr map map2 discard
map_location_data <- function(location = NULL,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              data = NULL,
                              url = NULL,
                              path = NULL,
                              package = NULL,
                              filetype = "gpkg",
                              fn = NULL,
                              crop = TRUE,
                              trim = FALSE,
                              from_crs = NULL,
                              crs = NULL,
                              class = "list",
                              label = NULL,
                              load = FALSE,
                              ...) {
  params <- rlang::list2(...)

  # FIXME: This may need some more checks to avoid passing data that would result in an error or other issue
  len_location <- length(location)
  len_data <- length(data)

  is_char_list_location <- ((len_location > 1) && is.character(location))
  is_list_location <- (is_sf_list(location, ext = TRUE) || is_char_list_location)

  if (is_char_list_location) {
    location <- as.list(location)
  }

  is_char_list_data <- ((len_data > 1) && is.character(data))
  is_list_data <- (is_sf_list(data) || is_char_list_data)


  if (is_char_list_data) {
    data <- as.list(data)


    if (is.null(names(data))) {
      label <- janitor::make_clean_names(label)

      data <-
        purrr::set_names(
          data,
          nm = purrr::map_chr(
            data,
            ~ paste0(c(label, janitor::make_clean_names(.x)), collapse = "_")
          )
        )
    }
  }

  if (is_list_data) {
    data <-
      purrr::map(
        data,
        ~ get_location_data(
          location = location,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          data = .x,
          package = package,
          filetype = filetype,
          fn = fn,
          crop = crop,
          trim = trim,
          from_crs = from_crs,
          crs = crs,
          locationname_col = params$locationname_col,
          locationname = params$locationname
        )
      )
  } else if (is_list_location) {
    data <-
      purrr::map(
        location,
        ~ get_location_data(
          location = .x,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          data = data,
          package = package,
          filetype = filetype,
          fn = fn,
          crop = crop,
          trim = trim,
          from_crs = from_crs,
          crs = crs,
          locationname_col = params$locationname_col,
          locationname = params$locationname
        )
      )
  } else if (is_list_data && is_list_location && (len_data == len_location)) {
    data <-
      purrr::map2(
        location,
        data,
        ~ get_location_data(
          location = .x,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          data = .y,
          package = package,
          filetype = filetype,
          fn = fn,
          crop = crop,
          trim = trim,
          from_crs = from_crs,
          crs = crs,
          locationname_col = params$locationname_col,
          locationname = params$locationname
        )
      )
  }

  data <- purrr::discard(~ nrow(.x) == 0)
  data <- as_sf_class(x = data, class = class, crs = crs, ...)

  if (load && is_sf_list(data, is_named = TRUE)) {
    list2env(data, envir = .GlobalEnv)
  } else {
    return(data)
  }
}
