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
#' @param ... Parameters passed to selected geom
#' @inheritParams get_location_data
#' @return ggplot2 geom
#' @seealso
#'  \code{\link[ggplot2]{CoordSf}}
#' @rdname layer_location_data
#' @export
#' @importFrom ggplot2 geom_sf geom_sf_text geom_sf_label
layer_location_data <-
  function(data = NULL,
           label,
           geom = "sf",
           location = NULL,
           dist = NULL,
           diag_ratio = NULL,
           asp = NULL,
           package = NULL,
           filetype,
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
        data = data,
        location = location,
        dist = dist,
        diag_ratio = diag_ratio,
        asp = asp,
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
        ...
      )
    }

    if (geom == "icon") {
      layer <- geom_sf_icon(
        data = data,
        ...
      )
    }

    if (geom == "text") {
      layer <- ggplot2::geom_sf_text(
        data = data,
        ...
      )
    }


    if (geom == "label") {
      layer <- ggplot2::geom_sf_label(
        data = data,
        ...
      )
    }

    return(layer)
  }