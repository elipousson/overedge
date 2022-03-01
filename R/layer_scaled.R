#' Create a ggplot2 layer scaled to a paper and orientation for a location
#'
#' Uses [layer_neatline], [standard_scales], and [get_standard_scale].
#'
#' @inheritParams get_standard_scale
#' @inheritParams st_bbox_ext
#' @inheritParams layer_neatline
#' @family layer
#' @name layer_scaled
#' @export
#' @importFrom usethis ui_stop
layer_scaled <-
  function(data = NULL,
           dist = NULL,
           diag_ratio = NULL,
           unit = NULL,
           asp = NULL,
           crs = NULL,
           scale = NULL,
           series = NULL,
           standard = NULL,
           paper = NULL,
           orientation = NULL,
           clip = FALSE) {

    # Get paper with actual width, height, and units
    scaled_paper <-
      get_standard_scale(
        paper = paper,
        orientation = orientation,
        series = series,
        standard = standard,
        scale = scale,
        convert = TRUE
      )

    if (is.null(asp)) {
      asp <- scaled_paper$asp
    }

    if (!check_sf(data, ext = TRUE)) {
      usethis::ui_stop("data must be a bounding box or simple feature object.")
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
      usethis::ui_stop("This data covers a larger area than can be displayed at this scale ({scale}) on this paper ({paper})")
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