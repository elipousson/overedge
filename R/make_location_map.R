#' Make a ggplot map using layer_location_data
#'
#' Location is used as the data parameter of layer_location_data so this
#' function is primarily appropriate for the layer_mapbox (`geom = "mapbox"`).
#'
#' @inheritParams get_paper
#' @param save If `TRUE`, save file with [ggsave_ext] or [ggsave_social],
#'   requires `basemap = TRUE` and filename is not NULL *or* ... include a name
#'   parameter. Default: FALSE
#' @inheritParams st_bbox_ext
#' @inheritParams layer_location_data
#' @inheritParams ggsave_ext
#' @param ... Additional parameters passed to [layer_location_data].
#' @rdname make_location_map
#' @export
#' @importFrom ggplot2 ggplot
make_location_map <- function(location,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              paper = NULL,
                              orientation = NULL,
                              geom = "sf",
                              basemap = TRUE,
                              below = NULL,
                              above = NULL,
                              save = FALSE,
                              name = NULL,
                              label = NULL,
                              prefix = NULL,
                              postfix = NULL,
                              filename = NULL,
                              device = NULL,
                              filetype = NULL,
                              path = NULL,
                              dpi = 300,
                              ...) {
  paper <-
    get_paper(
      paper = paper,
      orientation = orientation
    )

  if (is.null(asp)) {
    asp <- paper$section_asp
  }

  if (is.null(data)) {
    data <- location
    location <- NULL
  }

  if (!is.null(below) && is.list(below) && check_class(below[[1]], "ggproto")) {
    below_layer <- below
  } else {
    below_layer <- NULL
  }


  if (!is.null(above) && is.list(above) && check_class(above[[1]], "ggproto")) {
    above_layer <- above
  } else {
    above_layer <- NULL
  }

  map_layer <-
    list(
      below_layer,
      layer_location_data(
        data = data,
        location = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        geom = geom,
        ...
      ),
      above_layer
    )

  if (basemap) {
    map_layer <-
      ggplot2::ggplot() +
      map_layer
  }

  if (save && basemap) {
    ggsave_ext(
      plot = map_layer,
      width = paper$section_width,
      height = paper$section_height,
      filename = filename,
      name = name,
      label = label,
      prefix = prefix,
      postfix = postfix,
      device = device,
      filetype = filetype,
      path = path,
      scale = scale,
      dpi = dpi
    )
  }

  return(map_layer)
}

#' @name make_social_map
#' @rdname make_location_map
#' @inheritParams get_social_image
#' @export
#' @importFrom ggplot2 ggplot
make_social_map <- function(location,
                            dist = NULL,
                            diag_ratio = NULL,
                            unit = NULL,
                            asp = NULL,
                            crs = 3857,
                            image = NULL,
                            platform = NULL,
                            format = NULL,
                            orientation = NULL,
                            basemap = TRUE,
                            geom = "mapbox",
                            save = FALSE,
                            name = NULL,
                            filename = NULL,
                            label = NULL,
                            prefix = NULL,
                            postfix = NULL,
                            path = NULL,
                            filetype = "jpeg",
                            dpi = 72,
                            ...) {
  image_size <-
    get_social_image(
      image = image,
      platform = platform,
      format = format,
      orientation = orientation
    )

  if (is.null(asp)) {
    asp <- image_size$section_asp
  }

  bbox <-
    st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs
    )

  map_layer <-
    layer_location_data(
      data = bbox,
      geom = geom,
      ...
    )

  if (basemap) {
    map_layer <-
      ggplot2::ggplot() +
      map_layer
  }

  if (save && basemap) {
    ggsave_social(
      plot = map_layer,
      width = image_size$section_width,
      height = image_size$section_height,
      filename = filename,
      name = name,
      label = label,
      prefix = prefix,
      postfix = postfix,
      filetype = filetype,
      path = path,
      scale = scale,
      dpi = dpi
    )
  }

  return(map_layer)
}
