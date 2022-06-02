#' Layer location data into a ggplot2 map
#'
#' Helper function to make a ggplot2 layer from data returned by
#' [get_location_data()]. For text geoms, the required aesthetic mapping is
#' set based on the name_col but values passed to mapping take precedence.
#'
#' @details Supported geom function options:
#'
#' Options for the geom parameter from the overedge package include:
#'
#' - "icon" ([layer_icon]),
#' - "mapbox" ([layer_mapbox]),
#' - "markers" ([layer_markers]),
#' - "numbers" ([layer_numbers])
#'
#' Options for the geom parameter from other packages include:
#'
#'   - "textsf" ([geomtextpath::geom_textsf])
#'   - "labelsf" ([geomtextpath::geom_labelsf])
#'   - "text_repel" ([ggrepel::geom_text_repel])
#'   - "label_repel" ([ggrepel::geom_label_repel])
#'   - "mark" ([birdseyeview::layer_show_mark])
#'   - "location" ([birdseyeview::layer_show_location])
#'   - "context" ([birdseyeview::layer_show_context])
#'   - "pattern" ([ggpattern::geom_sf_pattern])
#'
#' Alternatively, use the "geom_fn" parameter to pass a function that returns a
#' ggplot2 layer to use instead of one of the preset geom functions.
#'
#' @param geom A character string indicating which ggplot2 geom to use, Default:
#'   'sf'. Options include "sf" ([ggplot2::geom_sf]), "icon" ([layer_icon]),
#'   "markers" ([layer_markers]), "text" ([ggplot2::geom_sf_text]), and "label"
#'   ([ggplot2::geom_sf_label]). See details for a full list.
#' @param geom_fn ggplot2 geom or custom function using lambda syntax. Use for
#'   passing custom mapping functions to layer_location_data beyond the
#'   supported geom options.
#' @param unit unit to adjust location by dist or diag_ratio; defaults to
#'   "meter"
#' @param label_col Column name or id for a column with the text or labels to
#'   pass to any text geom.
#' @param ... Parameters passed to selected geom
#' @inheritParams get_location_data
#' @inheritParams ggplot2::geom_sf
#' @return ggplot2 geom
#' @seealso
#'  [ggplot2::CoordSf()]
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
           geom = "sf",
           location = NULL,
           dist = getOption("overedge.dist"),
           diag_ratio = getOption("overedge.diag_ratio"),
           unit = getOption("overedge.unit", default = "meter"),
           asp = getOption("overedge.asp"),
           package = getOption("overedge.data_package"),
           filetype = getOption("overedge.data_filetype"),
           fn = NULL,
           geom_fn = NULL,
           crop = TRUE,
           trim = FALSE,
           from_crs = getOption("overedge.from_crs"),
           crs = getOption("overedge.crs"),
           label_col = "name",
           ...) {
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

    if (!is.null(geom_fn)) {
      return(use_fn(data, geom_fn))
    }

    overedge_geoms <- c("icon", "mapbox", "markers", "numbers")
    ggrepel_geoms <- c("text_repel", "label_repel")
    text_geoms <- c("text", "label", "textsf", "labelsf", ggrepel_geoms)
    birdseyeview_geoms <- c("mark", "location", "context")
    ggpattern_geoms <- "pattern"

    # Match geoms
    geom <- match.arg(
      geom,
      c("sf", overedge_geoms, text_geoms, birdseyeview_geoms, ggpattern_geoms)
    )

    is_geom_pkg_installed(geom)

    # Assign aesthetics for text/label geoms
    if (geom %in% text_geoms) {
      mapping <- modify_mapping(mapping = mapping, label = label_col)

      if (geom %in% ggrepel_geoms) {
        mapping <- modify_mapping(mapping = mapping, data = data)

        params <- c(
          params,
          stat = "sf_coordinates"
        )
      }
    }

    geom_chr <- geom

    geom <-
      switch(geom_chr,
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
        "context" = birdseyeview::layer_show_context,
        "pattern" = ggpattern::geom_sf_pattern
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

    if (geom_chr %in% c("markers", "numbers")) {
      params$geom <- geom_chr
    }

    if (!rlang::has_name(init_params, c("direction")) && (geom_chr %in% ggrepel_geoms)) {
      params$direction <- "both"
    }

    return(rlang::exec(geom, !!!params))
  }
