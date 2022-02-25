#' Layer a location border into a ggplot2 map
#'
#' Helper function to make a ggplot2 layer from data returned by
#' \code{get_location}
#'
#' @param data Data for location to show.
#' @param label label type (e.g. "text", "label")
#' @param color Color for location; defaults to "black".
#' @param linetype Line type for location; defaults to "dashed".
#' @param fill Fill for location; defaults to "NA".
#' @param ... Additional parameters passed to get_location if data is `NULL`.
#' @inheritParams get_location
#' @inheritParams layer_location_data
#' @inheritParams layer_mask
#' @return list of ggplot2 geoms
#' @seealso
#'  \code{\link[ggplot2]{CoordSf}}
#' @rdname layer_show_location
#' @export
#' @importFrom ggplot2 aes
#' @importFrom smoothr smooth
layer_show_location <-
  function(mapping = ggplot2::aes(),
           data = NULL,
           type = NULL,
           name = NULL,
           id = NULL,
           location = NULL,
           label_geom = NULL,
           name_col = "name",
           id_col = "id",
           index = NULL,
           label = NULL,
           union = FALSE,
           smooth = TRUE,
           crs = NULL,
           color = "black",
           linetype = "dashed",
           size = 2,
           fill = NA,
           dist = NULL,
           diag_ratio = NULL,
           unit = NULL,
           asp = NULL,
           mask = FALSE,
           neatline = FALSE,
           ...) {
    if (is.null(data)) {
      data <-
        get_location(
          type = type,
          name = name,
          id = id,
          location = location,
          label = label,
          name_col = name_col,
          id_col = id_col,
          index = index,
          union = union,
          ...
        )
    }

    if (smooth) {
      data <- smoothr::smooth(data)
    }

    location_layer <-
      layer_location_data(
        data = data,
        color = color,
        linetype = linetype,
        size = size,
        fill = fill
      )

    if (!is.null(label_geom)) {
      label_geom <- match.arg(label_geom, c("label", "text"))

      label_layer <-
        layer_location_data(
          data = data,
          color = "black",
          geom = label_geom,
          aes(label = .data[[name_col]])
        )

      location_layer <-
        list(
          location_layer,
          label_layer
        )
    }

    if (check_sf(mask, ext = TRUE)) {
      mask_layer <-
        layer_mask(
          data = data,
          mask = mask,
          crs = crs,
          neatline = neatline
        )
    } else if (mask) {
      mask_layer <-
        layer_mask(
          data = data,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          crs = crs,
          neatline = neatline
        )
    } else {
      mask_layer <- NULL
    }


    if (neatline) {
      neatline_layer <-
        layer_neatline(
          data = data,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          crs = crs
        )
    } else {
      neatline_layer <- NULL
    }

    location_layer <-
      list(
        location_layer,
        mask_layer,
        neatline_layer
      )

    return(location_layer)
  }
