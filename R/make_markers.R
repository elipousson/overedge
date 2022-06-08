#' Make map markers from a simple feature object
#'
#' @param data Data with markers, passed to data parameter of
#'   [get_location_data()]
#' @param groupname_col Group column name, used to join group metadata if
#'   group_meta is a non-spatial data frame; Default: `NULL`
#' @param group_meta Group metadata as a data frame or sf object that intersect
#'   with markers; Default: `NULL`
#' @param join The join function used by [sf::st_join()] if group_meta is
#'   an sf object, Default: [sf::st_intersects]
#' @param geocode If `TRUE`, geocode data using [tidygeocoder::geo] and then
#'   convert to sf with [df_to_sf], Default: `FALSE`
#' @param address_col Address column, used if geocode is `TRUE` Default:
#'   'address'
#' @param point If `TRUE`, convert geometry to "POINT" with [sf::st_centroid()],
#'   Default: `TRUE`
#' @param crs Coordinate reference system for markers, Default: `NULL`
#' @param fn Function to apply to data before results; gives warning if data is
#'   grouped; Default: `NULL`
#' @param ... Additional parameters passed to [get_location_data()]
#' @rdname make_markers
#' @export
#' @importFrom sf st_intersects st_join st_centroid
#' @importFrom dplyr filter left_join group_by
#' @importFrom rlang as_function
make_markers <- function(data,
                         groupname_col = NULL,
                         group_meta = NULL,
                         join = sf::st_intersects,
                         geocode = FALSE,
                         address_col = "address",
                         point = TRUE,
                         crs = NULL,
                         fn = NULL,
                         ...) {
  if (!geocode) {
    data <-
      get_location_data(
        data = data,
        crs = crs,
        ...
      )
  } else if ((address_col %in% names(data)) && is.data.frame(data)) {
    # FIXME: Figure out how to properly quo/enquo the address column name
    data$address <- as.character(data[[address_col]])

    is_pkg_installed("tidygeocoder")

    data <-
      tidygeocoder::geocode(
        data,
        address = address,
        long = "lon",
        lat = "lat",
        quiet = rlang::is_interactive()
      )

    data <- df_to_sf(data, coords = c("lon", "lat"), crs = crs)
  }

  if (!is.null(group_meta)) {
    if (is.data.frame(group_meta) && !is.null(groupname_col)) {
      data <-
        dplyr::left_join(data, group_meta, by = groupname_col)
    } else if (is_sf(group_meta) && is_sf(data)) {
      data <-
        sf::st_join(x = data, y = group_meta, join = join)
    }
  }

  if (!is.null(groupname_col)) {
    data <-
      dplyr::filter(data, !is.na(.data[[groupname_col]]))

    data <-
      dplyr::group_by(data, .data[[groupname_col]])
  }

  # Convert to POINT if any other geometry
  if (!all(is_geom_type(data, type = c("POINT", "MULTIPOINT"))) && point) {
    data <- suppressWarnings(sf::st_centroid(data))
  }

  if (!is.null(groupname_col) && !is.null(fn)) {
    cli::cli_warn("Function passed to fn is being applied to grouped data.")
  }

  use_fn(data, fn)
}
