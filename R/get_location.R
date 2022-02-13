#' Get location of specified type
#'
#' Filter by name or id or use a spatial filter based on an sf object or
#' geocoded street address. Optionally you can use an index function to match
#' the type to a predefined group of data sets.
#'
#' @param type sf object with type of location to return, e.g. list of
#'   neighborhoods. Type is a set of features to select from or a character
#'   string references a set of features and can be passed to
#'   \code{get_location_data}.
#' @param name location name to return
#' @param id location id to return. id is coerced to character or numeric to
#'   match the class of the id_col for type.
#' @param location address or sf object passed to \code{\link[sf]{st_filter}}.
#'   If location is an address, the string is geocoded using
#'   \code{\link[tidygeocoder]{geo}}) and then also used as a spatial filter.
#' @param label optional label added to "label" column; must be a length 1 or match the rows in the output location. If `union = TRUE`,
#'   using label is recommended. Default: NULL
#' @param name_col Column name in type with name values, Default: 'name' Required if name provided.
#' @param id_col Column name in type with id values, Default: 'id'. Required if id is provided.
#' @param index Optional list used to match type to data, Default: NULL
#' @param union If TRUE and location, union location geometry with
#'   \code{\link[sf]{st_union}} and combine names Default: FALSE
#' @param ... Additional parameters passed to \code{get_location_data} if type is character and index is NULL
#' @return A sf object (subset from type data)
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
#' @importFrom usethis ui_warn
#' @importFrom checkmate check_class
#' @importFrom sf st_crs st_filter st_as_sf st_union
#' @importFrom tidygeocoder geo
#' @importFrom knitr combine_words
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
  if (is.character(type)) {
    if (is.list(index)) {
      # Return data from index list if provided
      type <- index[[type]]
    } else if (!is.null(index)) {
      usethis::ui_warn("index must be a list or NULL. The index provided cannot be used.")
    }

    # If type is a string
    if (!("sf" %in% class(type))) {
      # Return data if type is a file path, url, or package data
      type <- get_location_data(data = type, ...)
    }
  } else {
    # If type is not a string it must be an sf object
    checkmate::check_class(type, "sf")
  }

  type_crs <- sf::st_crs(type)

  # If location is not provided
  if (is.null(location)) {
    if (!is.null(name)) {
      # Filter type by name
      location <- type[type[[name_col]] %in% name, ]
    } else if (!is.null(id)) {
      if (is.character(type[[id_col]])) {
        # Filter type by ID
        location <- type[type[[id_col]] %in% as.character(id), ]
      } else if (is.numeric(type[[id_col]])) {
        location <- type[type[[id_col]] %in% as.numeric(id), ]
      }
    }
  } else {
    # Check if location if it is a character (assume it is an address)
    if (is.character(location)) {
      # Geocode the address
      # TODO: Figure out how to support multiple addresses
      location <- tidygeocoder::geo(
        address = location,
        lat = "lat",
        long = "lon",
        mode = "single"
      )
      # Convert single address df to sf
      location <- df_to_sf(location, coords = c("lon", "lat"), crs = type_crs)
    }

    # Filter sf
    if (checkmate::test_class(location, "sf")) {
        location <- sf::st_filter(type, location)
    }
  }

  if (union && (nrow(location) > 1)) {
    location <- sf::st_as_sf(
      as.data.frame(
        name = as.character(knitr::combine_words(location[[name_col]])),
        geometry = sf::st_union(location)
      )
    )
  }

  if (!is.null(label)) {
    location$label <- label
  }

  if (!is.null(location)) {
    return(location)
  } else {
    usethis::ui_warn("No location is provided. Returning all types.")
    return(type)
  }
}
