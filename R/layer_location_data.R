#' Layer location data into a ggplot2 map
#'
#' Helper function to make a ggplot2 layer from data returned by
#' \code{get_location_data}
#'
#' @param label label for area (appended to data as a prefix if data is a
#'   string)
#' @param geom ggplot2 geom to use, Default: 'sf'. Options include "sf"
#'   (geom_sf), "icon" (geom_sf_icon), "text" (geom_sf_text), "label"
#'   (geom_sf_label)
#' @param unit unit to adjust location by dist or diag_ratio; defaults to "meter"
#' @param ... Parameters passed to selected geom
#' @inheritParams get_location_data
#' @inheritParams ggplot2::geom_sf
#' @return ggplot2 geom
#' @seealso
#'  \code{\link[ggplot2]{CoordSf}}
#' @rdname layer_location_data
#' @export
#' @importFrom ggplot2 aes geom_sf geom_sf_text geom_sf_label
layer_location_data <-
  function(mapping = ggplot2::aes(),
           data = NULL,
           label,
           geom = "sf",
           location = NULL,
           dist = NULL,
           diag_ratio = NULL,
           unit = "meter",
           asp = NULL,
           package = NULL,
           filetype = NULL,
           fn = NULL,
           crop = TRUE,
           trim = FALSE,
           from_crs = NULL,
           crs = NULL,
           ...) {

    if (is.character(data) && !missing(label)) {
      data <- paste0(collapse = "_", c(label, data))
    }

    data <-
      get_location_data(
        location = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        data = data,
        url = NULL,
        path = NULL,
        package = package,
        filetype = filetype,
        fn = fn,
        crop = crop,
        trim = trim,
        from_crs = from_crs,
        crs = crs,
      )

    geom <- match.arg(geom, c("sf", "icon", "text", "label"))

    if (geom == "sf") {
      layer <- ggplot2::geom_sf(
        data = data,
        mapping = mapping,
        ...
      )
    }

    if (geom == "icon") {
      layer <- geom_sf_icon(
        data = data,
        mapping = mapping,
        ...
      )
    }

    if (geom == "text") {
      layer <- ggplot2::geom_sf_text(
        data = data,
        mapping = mapping,
        ...
      )
    }


    if (geom == "label") {
      layer <- ggplot2::geom_sf_label(
        data = data,
        mapping = mapping,
        ...
      )
    }

    return(layer)
  }
