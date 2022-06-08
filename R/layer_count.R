
#' Count simple feature objects
#'
#' Count the number of simple feature objects within a geometry or by some
#' attribute contained in both the data and the corresponding boundary geometry.
#'
#' @param data Data frame or `sf` object, Default: `NULL`
#' @param boundaries Boundary data as an `sf` object, Default: `NULL`
#' @param by Character string to join data by if data is not an `sf` object,
#'   Default: `NULL`
#' @param join The join function used by [sf::st_join()] if data is an `sf`
#'   object, Default: [sf::st_intersects]
#' @param .id Count column, Default: "count"
#' @param ... Additional parameters (not in use)
#' @rdname count_features
#' @export
#' @importFrom sf st_intersects st_nearest_feature st_drop_geometry st_join st_as_sf
#' @importFrom purrr map_dfr
#' @importFrom dplyr pull left_join
count_features <- function(data = NULL,
                           boundaries = NULL,
                           join = NULL,
                           by = NULL,
                           .id = "count",
                           ...) {
  if (is_sf(data)) {
    if (is_sf(boundaries)) {
      boundaries <- as_sf_list(boundaries)
    }

    # If features is not count data

    if (is.null(join)) {
      if (all(sapply(boundaries, is_polygon) | sapply(boundaries, is_multipolygon))) {
        join <- sf::st_intersects
      } else {
        join <- sf::st_nearest_feature
      }
    }

    data <-
      purrr::map_dfr(
        # Taken from write_exif_keywords
        boundaries,
        ~ sf::st_drop_geometry(
          sf::st_join(
            data,
            .x,
            # dplyr::select(.x, dplyr::all_of(key_col)),
            join = join
          )
        ),
        .id = path
      )
  } else if (is.data.frame(data) && is_sf(boundaries)) {
    if (class(dplyr::pull(data, by)) != class(dplyr::pull(boundaries, by))) {

    }

    # If features is count data
    # If boundary data is provided
    data <-
      dplyr::left_join(
        data,
        boundaries,
        by = by
      )

    data <- sf::st_as_sf(data)
    data <- rename_sf_col(data)

    # Return boundary data geometry
  }

  data
}

#' @noRd
layer_count <- function(data = NULL,
                        location = NULL,
                        boundaries = NULL,
                        mapping = NULL,
                        alpha = 0.6,
                        label_geom = "label",
                        label_col = "count",
                        count = FALSE,
                        count_col = "count",
                        basemap = FALSE,
                        crop = FALSE,
                        ...) {
  if (count) {
    # FIXME: pass data to count_features if count is TRUE
    # data <- has_same_name_col(data)
  }

  map_layer <-
    layer_location_data(
      data = data,
      geom = "sf",
      location = location,
      mapping = ggplot2::aes(fill = .data[[count_col]]),
      alpha = alpha,
      crop = crop,
      ...
    )

  # FIXME: Should I add fg_layer and bg_layer options to layer_location_data?

  # FIXME: This needs to be an option but I'm not sure how
  if (is.character(data)) {
    map_layer <-
      list(
        map_layer,
        layer_location_data(
          data = boundaries,
          location = location,
          fill = NA,
          color = "gray70",
          size = 1,
          crop = crop
        )
      )
  }

  if (!is.null(label_geom)) {
    map_layer <-
      list(
        map_layer,
        layer_location_data(
          data = data,
          geom = label_geom,
          location = location,
          label_col = label_col,
          crop = crop,
          ...
        )
      )
  }

  return(map_layer)
}
