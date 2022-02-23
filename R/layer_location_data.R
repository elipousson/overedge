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
#' @param unit unit to adjust location by dist or diag_ratio; defaults to
#'   "meter"
#' @param label_col Column name or id for a column with the text or labels to
#'   pass to [ggrepel] functions.
#' @param ... Parameters passed to selected geom
#' @inheritParams get_location_data
#' @inheritParams ggplot2::geom_sf
#' @return ggplot2 geom
#' @seealso
#'  \code{\link[ggplot2]{CoordSf}}
#' @rdname layer_location_data
#' @export
#' @importFrom ggplot2 aes geom_sf geom_sf_text geom_sf_label
#' @importFrom geomtextpath geom_textsf geom_labelsf
#' @importFrom ggrepel geom_text_repel geom_label_repel
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
           label_col = "name",
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
        crs = crs
      )

    geom <- match.arg(
      geom,
      c("sf", "icon", "text", "label", "textsf", "labelsf", "label_repel", "text_repel")
    )

    layer <-
      switch(geom,
      "sf" = ggplot2::geom_sf(
        mapping = mapping,
        data = data,
        ...
      ),
      "icon" = geom_sf_icon(
        mapping = mapping,
        data = data,
        ...
      ),
      "text" = ggplot2::geom_sf_text(
        mapping = mapping,
        data = data,
        ...
      ),
      "label" = ggplot2::geom_sf_label(
        mapping = mapping,
        data = data,
        ...
      ),
      "labelsf" = geomtextpath::geom_textsf(
        mapping = mapping,
        data = data,
        ...
      ),
      "textsf" = geomtextpath::geom_labelsf(
        mapping = mapping,
        data = data,
        ...
      ),
      "text_repel" = ggrepel::geom_text_repel(
        mapping = ggplot2::aes(
          label = .data[[label_col]],
          geometry = .data[[attributes(data)$sf_column]]
        ),
        data = data,
        stat = "sf_coordinates",
        ...
      ),
      "label_repel" = ggrepel::geom_label_repel(
        data = data,
        mapping = ggplot2::aes(
          label = .data[[label_col]],
          geometry = .data[[attributes(data)$sf_column]]
        ),
        stat = "sf_coordinates",
        ...
      )
    )

    return(layer)
  }
