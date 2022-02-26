#' Create a mask layer based on a simple feature object
#'
#' Returns a mask for an area or areas as a simple feature object.
#'
#' @param data `sf`, `sfc`, or `bbox` object. If dist, diag_ratio, and/or asp are provided, data is adjusted to set the boundaries of the mask. If data is not provided, `mask` is required.
#' @inheritParams st_bbox_ext
#' @param fill mask fill color; defaults to "white"
#' @param color mask edge color; defaults to `NA`
#' @param alpha mask alpha/transparency; defaults to 0.5
#' @param mask A `sf` or `bbox` object to define the edge of the mask.
#'   \code{diag_ratio}, \code{dist}, and \code{asp} parameters are ignored if a
#'   \code{mask} is provided. defaults to `NULL`
#' @param neatline If TRUE, add \code{layer_neatline} with `expand = TRUE`; defaults to FALSE.
#' @param ... Additional parameters to pass to \code{\link[ggplot2]{geom_sf}}
#' @return  \code{\link[ggplot2]{geom_sf}} function.
#' @family layer
#' @export
#' @importFrom sf st_transform st_difference st_union
layer_mask <- function(data = NULL,
                       dist = NULL,
                       diag_ratio = NULL,
                       unit = NULL,
                       asp = NULL,
                       crs = NULL,
                       fill = "white",
                       color = NA,
                       alpha = 0.5,
                       mask = NULL,
                       neatline = FALSE,
                       ...) {
  # Check if mask is provided
  if (is.null(mask)) {
    # Get adjusted bbox
    mask <- st_bbox_ext(
      x = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs
    )
  }

  if (check_bbox(mask)) {
    mask <- sf_bbox_to_sf(mask)
  }

  if (!is.null(data)) {
    if (check_bbox(data)) {
      data <- sf_bbox_to_sf(data)
    }

    data <- st_transform_ext(data, crs)

    if (nrow(data) > 1) {
      data <- sf::st_union(data)
    }

    mask <- suppressWarnings(sf::st_difference(mask, data))
  }

  mask_layer <-
    layer_location_data(
      data = mask,
      geom = "sf",
      fill = fill,
      color = color,
      alpha = alpha,
      ...
    )

  neatline_layer <- NULL

  if (neatline && all(vapply(c(dist, diag_ratio, asp), is.null, TRUE))) {
    neatline_layer <-
      layer_neatline(
        data = mask,
        expand = TRUE
      )
  } else if (neatline) {
    neatline_layer <-
      layer_neatline(
        data = data,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = crs,
        expand = TRUE
      )
  }

  return(list(mask_layer, neatline_layer))
}
