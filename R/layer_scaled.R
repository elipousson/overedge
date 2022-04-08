#' Create a ggplot2 layer scaled to a paper and orientation for a location
#'
#' Uses [layer_neatline], [standard_scales], and [convert_dist_scale].
#'
#' @inheritParams convert_dist_scale
#' @inheritParams st_bbox_ext
#' @inheritParams layer_neatline
#' @param clip If `TRUE`, create scaled layer even if the data is cut off; defaults to `FALSE`.
#' @family layer
#' @name layer_scaled
#' @export
layer_scaled <-
  function(data = NULL,
           dist = NULL,
           diag_ratio = NULL,
           unit = NULL,
           asp = NULL,
           crs = NULL,
           scale = NULL,
           paper = NULL,
           orientation = NULL,
           clip = FALSE) {

    # Get paper with actual width, height, and units
    scaled_paper <-
      convert_dist_scale(
        paper = paper,
        orientation = orientation,
        scale = scale
      )

    stopifnot(
      nrow(scaled_paper) == 1
    )

    if (is.null(asp)) {
      asp <- scaled_paper$asp
    }

    if (!is_sf(data, ext = TRUE)) {
      cli::cli_abort("data must be a bounding box or simple feature object.")
    } else {
      # Get adjusted bounding box for data
      bbox <- st_bbox_ext(
        x = data,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = crs
      )
    }

    # Compare bbox xdist and ydist to actual dimensions
    ydist <- sf_bbox_ydist(bbox, units = TRUE)
    xdist <- sf_bbox_xdist(bbox, units = TRUE)
    check_xdist <- (xdist <= scaled_paper$width_actual)
    check_ydist <- (ydist <= scaled_paper$height_actual)
    fit_check <- (check_ydist && check_xdist)

    if (!clip && !fit_check) {
      cli::cli_abort("This data covers a larger area than can be displayed at this scale ({scale}) on this paper ({paper})")
    } else {
      center <- st_center(data, ext = TRUE)
      scaled_dist <- max(c(scaled_paper$width_actual, scaled_paper$height_actual)) / 2

      scaled_bbox <-
        st_bbox_ext(
          x = center$sf,
          dist = scaled_dist,
          asp = scaled_paper$asp,
          unit = scaled_paper$unit_actual,
          crs = crs
        )
    }

    scaled_layer <-
      layer_neatline(
        data = scaled_bbox,
        color = NA,
        bgcolor = "none",
        expand = TRUE
      )

    return(scaled_layer)
  }
