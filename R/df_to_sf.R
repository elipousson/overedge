#' @title Convert data frame with coordinates to simple feature object
#' @param x data frame with lat/lon coordinates in 1 combined or 2 separated columns
#' @param coords character string with names of longitude and latitude column or columns, Default: c('LONGITUD', 'LATITUDE')
#' @param into If coords is a single column name with both longitude and latitude, `into` is used as the names of the new columns that coords is separated into. Passed to tidyr::separate().
#' @param sep If coords is a single column name with both longitude and latitude, `sep` is used as the separator between coordinate values. Passed to tidyr::separate().
#' @param lonlat If coords are ordered as latitude, longitude order or if into is latitude, longitude order, set to FALSE. Default TRUE.
#' @param crs coordinate reference system for returned sf object, Default: 4326
#' @return sf object
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'   nc_df <- ggspatial::df_spatial(nc)
#'
#'   df_to_sf(nc_df, coords = c("x", "y"))
#'
#'   nc_df$xy <- paste(nc_df$x, nc_df$y, sep = ",")
#'
#'   df_to_sf(nc_df, coords = "xy", into = c("lon", "lat"))
#' }
#' }
#' @seealso
#'  \code{\link[ggspatial]{df_spatial}},
#'  \code{\link[sf]{st_as_sf}}
#'  \code{\link[stats19]{format_sf}}
#' @rdname df_to_sf
#' @export
#' @importFrom tidyr separate
#' @importFrom tidyselect all_of
#' @importFrom dplyr mutate across
#' @importFrom readr parse_number
#' @importFrom usethis ui_info
#' @importFrom sf st_transform st_as_sf
df_to_sf <- function(x,
                     coords = c("LONGITUD", "LATITUDE"),
                     into = NULL,
                     sep = ",",
                     lonlat = TRUE,
                     crs = 4326) {

  if ((length(coords) == 1) && !is.null(into) && (length(into) == 2)) {
    x <-
      tidyr::separate(x, col = tidyselect::all_of(coords), into = into, sep = sep)

    x <-
      dplyr::mutate(
        x,
        dplyr::across(
          .cols = tidyselect::all_of(into),
          ~ readr::parse_number(.x)
        )
      )

    coords <- into
  }

  if (lonlat) {
    longitude <- coords[[1]]
    latitude <- coords[[2]]
  } else {
    latitude <- coords[[1]]
    longitude <- coords[[2]]
  }

  # Check that lat/lon are numeric
  if (!is.numeric(x[[longitude]])) {
    x[[longitude]] <- as.double(x[[longitude]])
    x[[latitude]] <- as.double(x[[latitude]])
  }

  # Check for missing coordinates
  missing_coords <- is.na(x[[longitude]] | x[[latitude]])

  if (sum(missing_coords) > 0) {
    usethis::ui_info("{sum(missing_coords)} rows removed for missing coordinates.")
    # Exclude rows with missing coordinates
    x <- x[!missing_coords, ]
  }

  x <-
    sf::st_transform(
      sf::st_as_sf(
        x,
        coords = c(longitude, latitude),
        agr = "constant",
        crs = 4326,
        stringsAsFactors = FALSE,
        remove = FALSE
      ),
      crs
    )

  return(x)
}
