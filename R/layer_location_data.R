#' Layer location data into a ggplot2 map
#'
#' Helper function to make a ggplot2 layer from data returned by
#' `get_location_data`. For text geoms, the required aesthetic mapping is
#' set based on the name_col but values passed to mapping take precedence.
#'
#' @param label label for area (appended to data as a prefix if data is a
#'   string)
#' @param geom ggplot2 geom to use, Default: 'sf'. Options include "sf"
#'   (geom_sf), "icon" (layer_icon / geom_sf_icon), "text" (geom_sf_text), "label"
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
#' @importFrom rlang list2 fn_fmls is_missing exec
#' @importFrom ggplot2 geom_sf geom_sf_text geom_sf_label
#' @importFrom purrr discard
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

    params <-
      rlang::list2(...)

    data <- as_sf(data)

    ggrepel_geoms <- c("text_repel", "label_repel")
    text_geoms <- c("text", "label", "textsf", "labelsf", ggrepel_geoms)
    birdseyeview_geoms <- c("mark", "location", "context")
    overedge_geoms <- c("icon", "mapbox", "markers", "numbers")


    # Match geoms
    geom <- match.arg(
      geom,
      c("sf", overedge_geoms, text_geoms, birdseyeview_geoms)
    )

    is_geom_pkg_installed(geom)

    is_ggrepel_geom <- geom %in% ggrepel_geoms

    # Assign aesthetics for text/label geoms
    if (geom %in% text_geoms) {
      mapping <- modify_mapping(mapping = mapping, label = label_col)

      if (is_ggrepel_geom) {
        mapping <- modify_mapping(mapping = mapping, data = data)

        params <- c(
          params,
          stat = "sf_coordinates"
        )
      }
    }

    geom <-
      switch(geom,
        "sf" = ggplot2::geom_sf,
        "icon" = layer_icon,
        "mapbox" = layer_mapbox,
        "markers" = layer_markers,
        "numbers" = layer_numbers,
        "text" = ggplot2::geom_sf_text,
        "label" = ggplot2::geom_sf_label,
        "text_repel" = ggrepel::geom_text_repel,
        "label_repel" = ggrepel::geom_label_repel,
        "textsf" = geomtextpath::geom_textsf,
        "labelsf" = geomtextpath::geom_labelsf,
        "mark" = birdseyeview::layer_show_mark,
        "location" = birdseyeview::layer_show_location,
        "context" = birdseyeview::layer_show_context
      )

    init_params <- params

    params <-
      modify_fn_fmls(
        params = params,
        fn = geom,
        mapping = mapping,
        data = data,
        keep.null = TRUE
      )

    # FIXME: This does not seem like the best way of dealing with the default params issue
    if (any(rlang::has_name(init_params, c("nudge_x", "nudge_y")))) {
      params$position <- NULL
    } else {
      params$nudge_x <- NULL
      params$nudge_y <- NULL
    }

    if (!rlang::has_name(init_params, c("direction")) && is_ggrepel_geom) {
      params$direction <- "both"
    }

    layer <-
      rlang::exec(geom, !!!params)

    return(layer)
  }
