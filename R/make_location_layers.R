#' Make map layers from location and data lists
#'
#' @example examples/make_location_layers.R
#' @noRd
make_location_layers <- function(data = NULL,
                                 location = NULL,
                                 geom = NULL,
                                 mapping = NULL,
                                 params = NULL) {
  data <- as_sf_list(data)
  location <- as_sf_list(location, nm = "location")
  mapping <- enquo(mapping)

  c(
    data, location,
    list(
      "geom" = geom,
      "mapping" = mapping
      # params
    )
  )
}
