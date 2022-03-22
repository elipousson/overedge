#' Get location of a specified type based on name, id, or location
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
#' @param location An address or `sf` or `bbox` object passed to
#'   \code{\link[sf]{st_filter}}. Any valid address or addresses are geocoded
#'   with \code{\link[tidygeocoder]{geo}}), converted to a simple feature
#'   object, and then used as a spatial filter.
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
#' @param crs Coordinate reference system to return; defaults to NULL which
#'   returns data using the same coordinate reference system as the provided
#'   type of location.
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
                         name_col = "name",
                         id = NULL,
                         id_col = "id",
                         location = NULL,
                         index = NULL,
                         union = FALSE,
                         crs = NULL,
                         label = NULL,
                         class = "sf",
                         ...) {
  stopifnot(
    is_sf(type) || is.character(type) || (is.null(type) && is.list(index)),
    is.character(location) || is.null(location) || is_sf(location, ext = TRUE) || is.numeric(location),
    is.list(index) || is.null(index),
    is.logical(union)
  )

  # FIXME: This basically assumes that if an index is provided than type is coming from the index
  # This is fine but should be clearly documented
  if (is.list(index)) {
    if (!is.null(index$type) && is.null(type)) {
      type <- unique(index$type)
    } else if (is.character(type) || is.numeric(type)) {
      # Return data from index list if provided
      type <- index[[type]]
    }
  } else if (is.character(type) && (is.null(index))) {
    # FIXME: Again this only calls get_location_data for type if the index is NULL
    # If type is a string
    # Return data if type is a file path, url, or package data
    type <- get_location_data(data = type, ...)
  }

  params <- rlang::list2(...)

  type_crs <- sf::st_crs(type)

  # If location is not provided
  if (is.null(location)) {
    if (!is.null(name)) {
      type_name_col <- type[[name_col]]
      # Filter type by name
      location <- type[type_name_col %in% name, ]
    } else if (!is.null(id)) {
      type_id_col <- type[[id_col]]
      if (is.character(type_id_col)) {
        # Filter type by ID
        location <- type[type_id_col %in% as.character(id), ]
      } else if (is.numeric(type[[id_col]])) {
        location <- type[type_id_col %in% as.numeric(id), ]
      }
    }

    if (!is.null(location) && (nrow(location) == 0)) {
      usethis::ui_stop("The name/id did not match any location of the type provided.")
    }
  } else {
    # Check if location if it is a character (assume it is an address)
    if (is.character(location)) {
      # Geocode the address
      location <-
        tidygeocoder::geo(
          address = location,
          long = "lon",
          lat = "lat"
        )
      # Convert single address df to sf
      location <- df_to_sf(location, coords = c("lon", "lat"), crs = type_crs)
    }

    # Filter sf
    if (!is.null(location) && is_sf(location, ext = TRUE)) {
      location <- as_sf(location)
      location <- st_transform_ext(location, crs = type_crs)
      location <- sf::st_filter(type, location)
    }
  }

  if (union && (nrow(location) > 1) && !is.null(name_col)) {
    location <-
      # FIXME: as_sf does not currently account for the possibility of a dataframe with a valid geometry column
      sf::st_as_sf(
        data.frame(
          "{name_col}" := as.character(
            knitr::combine_words(words = location[[name_col]])
          ),
          "geometry" = sf::st_union(location)
        )
      )
  }

  if (is.null(location) && !is.null(type)) {
    location <- type

    if (!is.null(params$locationname_col) && (nrow(type) > 1)) {
      usethis::ui_warn("Returning all locations of this type.")
    }
  }

  if (!is.null(label)) {
    location$label <- label
  }

  if (!is.null(name)) {
    col <- name_col
  } else if (!is.null(id)) {
    col <- id_col
  } else if (!is.null(params$locationname)) {
    col <- locationname_col
  } else {
    col <- NULL
  }

  location <- as_sf_class(x = location, class = class, crs = crs, col = col)

  return(location)
}
