#' Get location of specified type
#'
#' Filter by name or id or use a spatial filter based on an sf object or
#' geocoded street address. Optionally you can use an index list to match the
#' type to a named list of URLs or sf objects.
#'
#' @param type Type of location to return. Type can be an sf object, e.g. a data
#'   frame with multiple neighborhoods or a character string that can be passed
#'   to \code{get_location_data}. If index is provided, character can also be a
#'   character string to match the name of a list.
#' @param name Location name to return.
#' @param id Location id to return. id is coerced to character or numeric to
#'   match the class of the id_col for type.
#' @param location An address or `sf` or `bbox` object passed to \code{\link[sf]{st_filter}}.
#'   Any valid address or addresses are geocoded with
#'   \code{\link[tidygeocoder]{geo}}), converted to a simple feature object, and
#'   then used as a spatial filter.
#' @param location An address, bounding box (`bbox`), or simple feature (`sf`)
#'   object passed to \code{\link[sf]{st_filter}}. Any valid address or
#'   addresses are geocoded with \code{\link[tidygeocoder]{geo}}), converted to
#'   a simple feature object, and then used as a spatial filter. `bbox` objects
#'   are converted using [sf_bbox_to_sf()]. Multiple addresses are supported.
#' @param label Label optionally added to "label" column; must be a length 1 or
#'   match the number of rows returned based on the other parameters. If `union = TRUE`,
#'   using label is recommended. Default: `NULL`
#' @param name_col Column name in type with name values, Default: 'name'
#'   Required if name provided.
#' @param id_col Column name in type with id values, Default: 'id'. Required if
#'   id is provided.
#' @param index Optional list used to match type to data, Default: `NULL`
#' @param union If `TRUE`, the location geometry is unioned with
#'   \code{\link[sf]{st_union}} and the names are combined into a single value.
#'   Default: `FALSE`.
#' @param ... Additional parameters passed to \code{get_location_data} if type
#'   is character and index is `NULL`.
#' @return A simple feature object from data provided to type.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'   # Check if type as sf object with name/id lookup works
#'   get_location(type = nc, name = "Warren", name_col = "NAME")
#'   get_location(type = nc, id = 37185, id_col = "FIPSNO")
#'
#'   # Type also supports a range of other formats including
#'
#'   # File path
#'   get_location(
#'     type = system.file("shape/nc.shp", package = "sf"),
#'     name = "Hertford",
#'     name_col = "NAME"
#'   )
#'
#'   # Index name (if a named list of datasets, urls, or paths is passed to index)
#'   get_location(
#'     type = "smaller",
#'     name = "Hertford",
#'     name_col = "NAME",
#'     index = list(
#'       "smaller" = dplyr::filter(nc, AREA <= 0.10),
#'       "larger" = dplyr::filter(nc, AREA > 0.15)
#'     )
#'   )
#'
#'   # url may require passing extra parameters to `get_location_data()`
#'   # In this example, no location information is passed to get_location() so it warns before returning all types
#'   get_location(
#'     type = "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Parks/FeatureServer/0",
#'     locationname_col = "NAME",
#'     locationname = "Chesapeake and Ohio Canal National Historic Park"
#'   )
#' }
#' }
#' @rdname get_location
#' @export
#' @importFrom sf st_crs st_filter st_as_sf st_union
#' @importFrom tidygeocoder geo
#' @importFrom knitr combine_words
#' @importFrom usethis ui_warn
#' @importFrom rlang list2
get_location <- function(type,
                         name = NULL,
                         id = NULL,
                         location = NULL,
                         label = NULL,
                         name_col = "name",
                         id_col = "id",
                         index = NULL,
                         union = FALSE,
                         ...) {
  stopifnot(
    check_sf(type) || is.character(type),
    is.character(location) || is.null(location) || check_sf(location, ext = TRUE),
    is.list(index) || is.null(index),
    is.logical(union)
  )

  if (is.character(type) && is.list(index)) {
    # Return data from index list if provided
    type <- index[[type]]
  } else if (is.character(type)) {
    # If type is a string
    # Return data if type is a file path, url, or package data
    type <- get_location_data(data = type, ...)
  }

  dots <- rlang::list2(...)

  if (any(c("locationname_col", "locationname") %in% names(dots))) {
    warn <- FALSE
  } else {
    warn <- TRUE
  }

  type_crs <- sf::st_crs(type)

  # If location is not provided
  if (is.null(location)) {
    if (!is.null(name)) {
      # Filter type by name
      location <- type[type[[name_col]] %in% name, ]
      # FIXME: Add warning or error message if provided name does not return a location
    } else if (!is.null(id)) {
      if (is.character(type[[id_col]])) {
        # Filter type by ID
        location <- type[type[[id_col]] %in% as.character(id), ]
      } else if (is.numeric(type[[id_col]])) {
        location <- type[type[[id_col]] %in% as.numeric(id), ]
      }
      # FIXME: Add warning or error message if provided id does not return a location
    }
  } else {
    # Check if location if it is a character (assume it is an address)
    if (is.character(location)) {
      # Geocode the address
      location <- tidygeocoder::geo(
        address = location,
        long = "lon",
        lat = "lat"
      )
      # Convert single address df to sf
      location <- df_to_sf(location, coords = c("lon", "lat"), crs = type_crs)
    }

    # Filter sf
    if (check_sf(location, ext = TRUE)) {
      location <- as_sf(location)

      location <- sf::st_filter(type, location)
    }
  }

  if (union && (nrow(location) > 1)) {
    location <-
      sf::st_as_sf(
        data.frame(
          "name" = as.character(
            knitr::combine_words(words = location[[name_col]])
          ),
          "geometry" = sf::st_union(location)
        )
      )
  }

  if (!is.null(label)) {
    location$label <- label
  }

  if (!is.null(location)) {
    return(location)
  } else {
    if (warn) {
      usethis::ui_warn("Returning all locations of this type.")
    }

    return(type)
  }
}
