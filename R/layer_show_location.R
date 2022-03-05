#' Layer a location border into a ggplot2 map
#'
#' Helper function to make a ggplot2 layer from data returned by
#' \code{get_location}. See `birdseyeview::layer_show_location` for documentation.
#'
#' @return list of ggplot2 geoms
#' @inheritParams get_location
#' @inheritParams layer_location_data
#' @inheritParams layer_mask
#' @seealso
#'  \code{\link[ggplot2]{CoordSf}}
#' @rdname layer_show_location
#' @family layer
#' @export
#' @importFrom ggplot2 aes
#' @importFrom smoothr smooth
layer_show_location <-
  birdseyeview::layer_show_location
