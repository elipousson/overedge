#' Use mapboxapi to make a Mapbox static map layer
#'
#' layer_mapbox works the same as make_mapbox_basemap but defaults to
#' basemap and neatline to FALSE
#'
#' @param data sf, sfc, or bbox object; any objects convertible with [as_bbox()]
#' @inheritParams layer_neatline
#' @inheritParams mapboxapi::layer_static_mapbox
#' @param map_style Map style used to fill style_id and username parameters,
#'   Default: "mapbox://styles/mapbox/satellite-streets-v11"
#' @param basemap If FALSE, create a standalone layer; if TRUE, the layer is
#'   precededed by [ggplot2::ggplot()] to allow use as a basemap, Default: TRUE
#' @param neatline If TRUE, add a neatline matching the provided data, Default: TRUE
#' @inheritParams layer_neatline
#' @param ... Additional parameter passed to [mapboxapi::layer_static_mapbox]
#' @seealso
#' \code{\link[mapboxapi]{layer_static_mapbox}}
#'  \code{\link[overedge]{st_transform_ext}},\code{\link[overedge]{as_sf}},\code{\link[overedge]{layer_neatline}}
#' @rdname layer_mapbox
#' @md
#' @export
#' @importFrom stringr str_extract
#' @importFrom mapboxapi layer_static_mapbox
#' @importFrom ggplot2 ggplot
layer_mapbox <- function(data = NULL,
                         dist = NULL,
                         diag_ratio = NULL,
                         unit = "meter",
                         asp = NULL,
                         style_url = "mapbox://styles/mapbox/satellite-streets-v11",
                         style_id = NULL,
                         username = NULL,
                         basemap = FALSE,
                         scale = 0.75,
                         scaling_factor = "1x",
                         attribution = TRUE,
                         logo = TRUE,
                         access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
                         neatline = TRUE,
                         color = "black",
                         bgcolor = "white",
                         size = 1,
                         linetype = "solid",
                         expand = FALSE,
                         hide_grid = TRUE,
                         label_axes = "----",
                         ...) {
  if (!is.null(style_url)) {
    username <- stringr::str_extract(style_url, "(?<=styles/).+(?=/)")
    style_id <- stringr::str_extract(style_url, glue::glue("(?<={username}/).+"))
  }

  # Set appropriate CRS for Mapbox
  crs_mapbox <- 3857

  bbox <-
    st_bbox_ext(
      x = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs_mapbox
    )

  # Get Mapbox map
  mapbox_layer <-
    mapboxapi::layer_static_mapbox(
      location = bbox,
      buffer_dist = 0,
      style_id = style_id,
      username = username,
      scale = scale,
      scaling_factor = scaling_factor,
      attribution = attribution,
      logo = logo,
      access_token = access_token,
      ...
    )

  if (neatline) {
    mapbox_layer <-
      list(
        mapbox_layer,
        layer_neatline(
          data = bbox,
          crs = crs_mapbox,
          color = color,
          bgcolor = bgcolor,
          size = size,
          linetype = linetype,
          expand = expand,
          hide_grid = hide_grid,
          label_axes = label_axes
        )
      )
  }

  if (basemap) {
    mapbox_layer <-
      ggplot2::ggplot() +
      mapbox_layer
  }

  return(mapbox_layer)
}
