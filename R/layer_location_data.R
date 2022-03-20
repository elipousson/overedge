#' Layer location data into a ggplot2 map
#'
#' Helper function to make a ggplot2 layer from data returned by
#' \code{get_location_data}. For text geoms, the required aesthetic mapping is
#' set based on the name_col but can be values passed to mapping take
#' precedence.
#'
#' @param label label for area (appended to data as a prefix if data is a
#'   string)
#' @param geom ggplot2 geom to use, Default: 'sf'. Options include "sf"
#'   (geom_sf), "icon" (geom_sf_icon), "text" (geom_sf_text), "label"
#'   (geom_sf_label), "textsf", "labelsf", "text_repel", and "label_repel".
#' @param unit unit to adjust location by dist or diag_ratio; defaults to
#'   "meter"
#' @param label_col Column name or id for a column with the text or labels to
#'   pass to any text geom.
#' @param ... Parameters passed to selected geom
#' @inheritParams get_location_data
#' @inheritParams ggplot2::geom_sf
#' @return ggplot2 geom
#' @seealso
#'  \code{\link[ggplot2]{CoordSf}}
#' @rdname layer_location_data
#' @family layer
#' @export
#' @importFrom ggplot2 aes geom_sf geom_sf_text geom_sf_label
#' @importFrom utils modifyList
layer_location_data <-
  function(mapping = NULL,
           data = NULL,
           label = NULL,
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

    # FIXME: The use of label in this function is designed for use with batch loading
    # but it is inconsistent with how other functions handle the "label" parameter
    # if (is.character(data) && !is.null(label)) {
    #  data <- paste0(collapse = "_", c(label, data))
    # }

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
        class = "sf",
        ...
      )

    data <- as_sf(data)

    text_geoms <- c("text", "label", "textsf", "labelsf", "text_repel", "label_repel")
    ggrepel_geoms <- c("text_repel", "label_repel")

    # Match geoms
    geom <- match.arg(
      geom,
      c("sf", "icon", text_geoms)
    )

    # Check if packages are available for text/label geoms
    if (geom %in% c("textsf", "labelsf")) {
      check_pkg_installed("geomtextpath")
    }

    # Assign aesthetics for text/label geoms
    if (geom %in% text_geoms) {
      mapping <- modify_mapping(mapping = mapping, label = label_col)

      if (geom %in% ggrepel_geoms) {
        check_pkg_installed("ggrepel")
        mapping <- modify_mapping(mapping = mapping, data = data)
        stat <- "sf_coordinates"
      }
    }

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
        "text_repel" = ggrepel::geom_text_repel(
          mapping = mapping,
          data = data,
          stat = stat,
          ...
        ),
        "label_repel" = ggrepel::geom_label_repel(
          data = data,
          mapping = mapping,
          stat = stat,
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
        )
      )

    return(layer)
  }
