#' @title Draw SVG icons for simple feature objects
#' @description Use the \code{\link[ggsvg]{geom_point_svg()}} function to plot
#'   icons using the centroids from the input simple feature object to set the
#'   icon location.
#' @param data sf object. If the geometry type for data is POINT, the object is
#'   used as is. If not, the object is converted to POINT using
#'   \code{\link[sf]{st_centroid()}}.
#' @param iconname_col The column name in the input data to use as the icon
#'   name. If the name matches multiple icons, the first match from `map_icons`
#'   is used. You may provide a px or source value to select a different match
#'   if needed but, in that case, all icons must use the same px or source
#'   value. Note that the icon column should not be mapped with `aes()`.
#' @param icon Icon name. Default NULL. If `icon` is provided, `iconname_col` is
#'   not used.
#' @param px Icon size in pixels. See `map_icons$px` for supported options.
#'   Optional but may be necessary to differentiate icons with duplicate names.
#' @param source Icon source. See `map_icons$repo` for supported options.
#'   Optional but may be required to differentiate icons with duplicate names.
#' @param svg Optional. Custom file path or URL with SVG to pass to `svg`
#'   parameter for `ggsvg::geom_point_svg()`.  If `icon` is provided, `svg` is
#'   not used.
#' @param color SVG color passed to ggsvg::geom_point_svg(). default color is set to "black".
#' @param ... Additional parameters to ggsvg::geom_point_svg()
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(sf)
#'   library(ggplot2)
#'   library(overedge)
#'
#'   nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#'   ggplot() +
#'     geom_sf_icon(data = nc, icon = "point-start", size = 10)
#'
#'
#'   nc$icon <- rep(c("1", "2", "3", "4"), nrow(nc) / 4)
#'   ggplot() +
#'     geom_sf_icon(data = nc, size = 5)
#' }
#' }
#' @seealso
#'  \code{\link[ggsvg]{geom_point_svg}}
#'  \code{\link[overedge]{map_icons}}
#' @rdname geom_sf_icon
#' @export
#' @importFrom sf st_geometry_type st_centroid st_coordinates
#' @importFrom usethis ui_warn ui_stop
#' @importFrom dplyr left_join rename bind_cols filter
#' @importFrom ggsvg geom_point_svg
#' @importFrom ggplot2 aes
#' @importFrom stringr str_detect
geom_sf_icon <- function(data = NULL,
                         iconname_col = "icon",
                         icon = NULL,
                         px = NULL,
                         source = NULL,
                         svg = NULL,
                         color = "black",
                         ...) {
  geometry_type <- as.character(sf::st_geometry_type(data, by_geometry = FALSE))

  if (geometry_type != "POINT") {
    usethis::ui_warn("Converting data from {geometry_type} to POINT with `sf::st_centroid()`.")
    data <-
      suppressWarnings(sf::st_centroid(data))
  }

  coord_df <- get_coord_df(data)

  if ((iconname_col %in% names(data)) && is.null(icon)) {
    icon_options <- dplyr::rename(map_icons, svg_url = url, {{ iconname_col }} := name)

    if (!is.null(px)) {
      icon_options <- dplyr::filter(icon_options, px == px)
    }

    if (!is.null(source)) {
      icon_options <- dplyr::filter(icon_options, repo == source)
    }

    data <-
      dplyr::left_join(
        data,
        icon_options,
        by = {{ iconname_col }}
      )

    data <-
      dplyr::bind_cols(
        data,
        coord_df
      )

    ggsvg::geom_point_svg(
      data = data,
      ggplot2::aes(x = X, y = Y, svg = svg_url),
      ...
    )
  } else if (!is.null(icon)) {
    icon <- dplyr::filter(map_icons, name == icon)

    if (!is.null(px)) {
      icon <- dplyr::filter(icon, size == px)
    }

    if (!is.null(source)) {
      icon <- dplyr::filter(icon, stringr::str_detect(repo, source))
    }

    if (nrow(icon) == 1) {
      ggsvg::geom_point_svg(
        data = coord_df,
        ggplot2::aes(x = X, y = Y),
        svg = icon$url,
        color = color,
        defaults = list(color = "black"),
        ...
      )
    } else {
      usethis::ui_stop("The provided parameters match more than one icon.
                        Provide the `px` and/or `source` to select a single icon.")
    }
  } else if (!is.null(svg)) {
    ggsvg::geom_point_svg(
      data = coord_df,
      ggplot2::aes(x = X, y = Y),
      svg = svg,
      color = color,
      ...
    )
  }
}
