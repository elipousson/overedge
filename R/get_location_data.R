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
#'   into a single sf object using [sf::st_union]
#' @inheritParams st_bbox_ext
#' @param data sf object including data in area. data may also be a url or file
#'   path. data can be the name of a data object or, if package and filetype are
#'   provided, a cached or external file.
#' @param url url for FeatureServer or MapServer layer to pass to
#'   [get_esri_data]. url can be provided to data parameter
#' @param path path to spatial data file supported by [sf::read_sf]
#' @param package package name.
#' @param filetype file type supported by [sf::read_sf]. The file type must be
#'   provided for extdata and cached data.
#' @param fn Function to apply to data before returning.
#' @inheritParams location_filter
#' @param from_crs coordinate reference system of the data.
#' @param crs coordinate reference system to return
#' @param class Class of object to return.
#' @param index A list of possible location, data, and (optionally) package
#'   values. List must be named and include a value named package and package
#'   must be `NULL`, to set package based on index. If list is not NULL and
#'   location and/or data as character or numeric values, the location and data
#'   are assumed to be index values for the index list. The index parameter
#'   supports nested lists created by [make_location_data_list] (using only the
#'   default key names of "location" and "data"). This feature has not be fully
#'   tested and may result in errors or unexpected results.
#' @param label label is optionally used by [map_location_data] to name the data
#'   objects in the list returned by the function.
#' @param ... additional parameters passed to [read_sf_path], [read_sf_url], or
#'   [read_sf_pkg] and [location_filter]
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
                              index = NULL,
                              ...) {
  if (!is.null(index) && is.list(index)) {
    # FIXME: This is set to work with 1 or 2 level list indices with naming conventions that match make_location_data_list
    # This should be clearly documented as alternate index naming conventions supported if possible
    if (("package" %in% names(index)) && is.null(package)) {
      package <- unique(index$package) # could use data as an index
      stopifnot(length(package) == 1)
    }

    lookup_location <- (!is.null(location) && (is.character(location) || is.numeric(location)))

    # Return data from index list if provided (may include bbox, sfc, or sf objects)
    if (lookup_location) {
      if ("location" %in% names(index)) {
        location <- index$location[[location]]
      } else {
        location <- index[[location]]
      }
    }

    lookup_data <- (!is.null(data) && (is.character(data) || is.numeric(data)))

    # Return data from index list if provided (may include character (e.g. url, file path, data name if in package), bbox, sfc, or sf objects)
    if (lookup_data) {
      if ("data" %in% names(index)) {
        data <- index$data[[data]]
      } else {
        data <- index[[data]]
      }
    }
  }

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
    # FIXME: cannot pass the bbox via params but that is OK
    # If a bounding box is not in the params this should pass NULL but I need to double-check
    bbox <- NULL # params$bbox
  }

  if (is_bbox(data)) {
    data <- as_sf(data)
  }

  # Check if data is not an  sf object
  # FIXME: The read_sf_ext function I started handles this type of checking and switching
  if (!is_sf(data)) {

    # Check if data is a url
    if (is_url(data)) {
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

  # FIXME: Document how the filter works
  data <-
    location_filter(
      data = data,
      location = location,
      bbox = bbox,
      trim = trim,
      crop = crop,
      ...
    )

  data <- use_fn(data = data, fn = fn)

  params <- rlang::list2(...)

  if (!is.null(params$col)) {
    col <- params$col
  } else {
    col <- NULL
  }

  # TODO: Is the following pattern of setting col based on ... and converting the class something that should be pulled into a separate utility function?
  data <- as_sf_class(x = data, class = class, crs = crs, col = col)

  return(data)
}

#' @name map_location_data
#' @rdname get_location_data
#' @param load If TRUE and class is "list", load data to local environment; defaults FALSE.
#' @example examples/map_location_data.R
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
                              index = NULL,
                              ...) {
  # FIXME: This triggers an alert with lintr but it is used
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

    # FIXME: The addition of a index parameter to get_location_data should allow the use of the index as a secondary source of name data for map_location_data
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
          name_col = params$name_col,
          name = params$name,
          index = index
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
          name_col = params$name_col,
          name = params$name,
          index = index
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
          name_col = params$name_col,
          name = params$name,
          index = index
        )
      )
  }

  data <- purrr::discard(data, ~ nrow(.x) == 0)
  data <- as_sf_class(x = data, class = class, crs = crs, ...)

  if (load && is_sf_list(data, is_named = TRUE)) {
    list2env(data, envir = .GlobalEnv)
  } else {
    return(data)
  }
}
