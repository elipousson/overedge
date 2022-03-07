#' Get data for a location
#'
#' Returns data for a selected area or areas with an optional buffer. If both
#' crop and trim are FALSE, the function uses \code{\link[sf]{st_intersects}} to
#' filter data to include the full geometry of anything that overlaps with the
#' area or bbox (if the area is not provided).
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
#' @param ... additional parameters passed to read_sf_path, read_sf_url, or read_sf_package
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
                              filetype,
                              fn = NULL,
                              crop = TRUE,
                              trim = FALSE,
                              from_crs = NULL,
                              crs = NULL,
                              ...) {
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

  # Check if data is in local environment
  if ((length(data) == 1) && (data %in% ls(envir = .GlobalEnv)) && !check_sf(data)) {
    if (usethis::ui_yeah("Do you want to load {data} from the global environment?")) {
      data <- eval_data(data = data)
    }

    if (!check_sf(data)) {
      usethis::ui_warn("The loaded data is not an sf object.")
    }
  }

  # Check if data is not an  sf object
  if (!(check_sf(data))) {

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
      data <- read_sf_package(data = data, bbox = bbox, package = package, filetype = filetype, ...)
    }
  }

  if (!is.null(bbox)) {
    if (trim) {

      # If trim, match location crs to data
      location <- st_transform_ext(x = location, crs = data)
      data <- suppressWarnings(sf::st_intersection(data, location))
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

  if (!is.null(crs)) {
    data <- sf::st_transform(data, crs)
  }

  return(data)
}
