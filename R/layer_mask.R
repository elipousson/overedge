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
#'   `diag_ratio`, `dist`, and `asp` parameters are ignored if a
#'   `mask` is provided. defaults to `NULL`
#' @param neatline If TRUE, add `layer_neatline` with `expand = TRUE`; defaults to FALSE.
#' @param ... Additional parameters to pass to [ggplot2::geom_sf()]
#' @return  [ggplot2::geom_sf()] function.
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
      crs = crs,
      class = "sf"
    )
  } else {
    # Convert mask to sf if needed
    mask <- as_sf(mask)
  }

  if (!is.null(data)) {
    # Erase data from mask
    data <- st_transform_ext(data, crs, class = "sf")
    mask <- st_erase(x = mask, y = data)
  }

  # Create mask layer
  mask_layer <-
    layer_location_data(
      data = mask,
      geom = "sf",
      fill = fill,
      color = color,
      alpha = alpha,
      ...
    )

  # Create neatline layer
  neatline_layer <- NULL

  if (neatline && all(vapply(c(dist, diag_ratio, asp), is.null, TRUE))) {
    # FIXME: This option is not documented and may not be expected behavior in all cases
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
