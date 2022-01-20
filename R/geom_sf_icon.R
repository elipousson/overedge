#' @title Draw SVG icons for sf objects
#' @description Use the `ggsvg::geom_point_svg()` function to plot icons at coordinates for an sf object.
#' @param data sf object. If the geometry type for data is POINT, the object is
#'   used as is. If not, the object is converted to POINT using
#'   sf::st_centroid(). If the sf object has a column named "icon" and `icon` is
#'   NULL, the value of the column will be used as the icon name.
#' @param icon Icon name. If the data includes a column named icon, `icon` is optional. Otherwise, `icon` is required.
#' @param px Icon size in pixels. Optional but may be required to differentiate icons with duplicate names.
#' @param source Icon source. Optional but may be required to differentiate icons with duplicate names.
#' @param ... Parameters to ggsvg::geom_point_svg()
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(sf)
#'  library(ggplot2)
#'  library(overedge)
#'
#'  nc <- st_read(system.file("shape/nc.shp", package="sf"))
#'  ggplot() +
#'   geom_sf_icon(data = nc, icon = "point-start", size = 10)
#'
#'
#'  nc$icon <- rep(c("1", "2", "3", "4"), nrow(nc)/4)
#'  ggplot() +
#'    geom_sf_icon(data = nc, size = 5)
#'  }
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
geom_sf_icon <- function(data = NULL, icon = NULL, px = NULL, source = NULL, svg = NULL, ...) {
  geometry_type <- as.character(sf::st_geometry_type(data, by_geometry = FALSE))

  if (geometry_type != "POINT") {
    usethis::ui_warn("Converting data from {geometry_type} to POINT with `sf::st_centroid()`.")
    data <-
      suppressWarnings(sf::st_centroid(data))
  }

  if (!is.null(icon)) {
    icon <-
      dplyr::filter(overedge::map_icons, name == icon)

    if (!is.null(px)) {
      icon <- dplyr::filter(icon, size == px)
    }

    if (!is.null(source)) {
      icon <- dplyr::filter(icon, stringr::str_detect(repo, source))
    }

    if (nrow(icon) == 1) {
      ggsvg::geom_point_svg(
        data = as.data.frame(sf::st_coordinates(data)),
        ggplot2::aes(x = X, y = Y),
        svg = icon$url,
        ...
      )
    } else {
      usethis::ui_stop("Provided parameters match more than one icon. Provide the `px` or `source` to select a single icon for display.")
    }
  } else if ("icon" %in% names(data)) {
    data <-
      dplyr::left_join(
        data,
        dplyr::rename(overedge::map_icons, icon_svg = url),
        by = c("icon" = "name")
      )

    ggsvg::geom_point_svg(
      data = dplyr::bind_cols(
        as.data.frame(sf::st_coordinates(data)),
        "icon_svg" = data$icon_svg
      ),
      ggplot2::aes(x = X, y = Y, svg = icon_svg),
      ...
    )
  }
}
