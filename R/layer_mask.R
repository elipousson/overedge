#' Create a mask layer based on an sf object
#'
#' Returns a mask for an area or areas as an  \code{sf} object.
#'
#' @param data sf, sfc, or bbox object. If dist, diag_ratio, and/or asp are provided, data is adjusted to set the boundaries of the mask. If data is not provided, `mask` is required.
#' @inheritParams st_bbox_adj
#' @param fill mask fill color; defaults to "white"
#' @param color mask edge color; defaults to NA
#' @param alpha mask alpha/transparency; defaults to 0.5
#' @param mask \code{bbox} object to define the edge of the mask.
#'   \code{diag_ratio}, \code{dist}, and \code{asp} parameters are ignored if a
#'   \code{mask} is provided. defaults to NULL
#' @param ... Additional parameters to pass to \code{\link[ggplot2]{geom_sf}}
#' @return  \code{\link[ggplot2]{geom_sf}} function.
#' @export
#' @importFrom sf st_transform st_difference st_union
layer_mask <- function(data = NULL,
                       dist = NULL,
                       diag_ratio = NULL,
                       asp = NULL,
                       crs = NULL,
                       fill = "white",
                       color = NA,
                       alpha = 0.5,
                       mask = NULL,
                       ...) {
  # Check if mask is provided
  if (is.null(mask)) {
    # Get adjusted bbox
    mask <- st_bbox_adj(
      x = data,
      dist = dist,
      diag_ratio = diag_ratio,
      asp = asp,
      crs = crs
    )
  }

  if ("bbox" %in% class(mask)) {
    mask <- sf_bbox_to_sf(mask)
  }

  if (!is.null(data)) {
    if ("bbox" %in% class(data)) {
      data <- sf_bbox_to_sf(data)
    }

    if (sf::st_crs(data) != sf::st_crs(crs)) {
      data <- sf::st_transform(data, crs)
    }

    mask <- sf::st_difference(mask, sf::st_union(data))
  }

  mask_layer <- layer_sf_data(data = mask, fill = fill, color = color, alpha = alpha, ...)

  return(mask_layer)
}
