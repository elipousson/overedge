#' Make a ggplot map using layer_location_data
#'
#' @inheritParams get_paper
#' @param save If `TRUE`, save file with [ggsave_ext] or [ggsave_social], requires `basemap = TRUE` and filename is not NULL *or* ... include a name parameter. Default: FALSE
#' @inheritParams layer_location_data
#' @inheritParams mapboxapi::layer_static_mapbox
#' @param ... Additional parameters passed to [ggsave_social].
#' @rdname make_location_map
#' @export
#' @importFrom ggplot2 ggplot
make_location_map <- function(location,
                              data = NULL,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              paper = NULL,
                              orientation = NULL,
                              geom = "sf",
                              basemap = TRUE,
                              save = FALSE,
                              name = NULL,
                              label = NULL,
                              prefix = NULL,
                              postfix = NULL,
                              filename = NULL,
                              device = NULL,
                              filetype = NULL,
                              path = NULL,
                              scale = 1,
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

  map_layer <-
    layer_location_data(
      data = data,
      location = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      geom = geom,
      ...
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
                            data = NULL,
                            dist = NULL,
                            diag_ratio = NULL,
                            unit = NULL,
                            asp = NULL,
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

  if (is.null(data)) {
    data <- location
    location <- NULL
  }

  map_layer <-
    layer_location_data(
      data = data,
      location = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
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
