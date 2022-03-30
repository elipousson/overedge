#' Check ggplot objects and ggplot lists
#'
#' Functions for checking on if an object is a ggplot2 object and what type it
#' is. These functions may be removed if alternatives can be identified in a
#' different package.
#'
#' @name is_gg
NULL

#' @name is_gg_layer
#' @rdname is_gg
#' @param x A object to check if it is a ggplot object.
#' @export
#' @importFrom ggplot2 is.ggproto
is_gg_layer <- function(x) {
  is.list(x) && is.ggproto(x[[1]])
}

#' @name is_gg_sf_layer
#' @rdname is_gg
#' @export
is_gg_sf_layer <- function(x) {
  is.list(x) &&
    ((any("LayerSf" %in% sapply(x[[1]], class)) ||
      any("CoordSf" %in% sapply(x[[1]], class))) ||
      (any("LayerSf" %in% sapply(x, class)) ||
        any("CoordSf" %in% sapply(x, class)))
    )
}

#' @name is_gg_scale
#' @rdname is_gg
#' @export
#' @importFrom ggplot2 is.ggproto
is_gg_scale <- function(x) {
  (is_class(x, "Scale") && ggplot2::is.ggproto(x)) ||
    (is_class(x[[1]], "Scale") && ggplot2::is.ggproto(x[[1]]))
}

#' @name is_gg_theme
#' @rdname is_gg
#' @export
is_gg_theme <- function(x) {
  is_class(x, "theme") && is_class(x, "gg") ||
    is_class(x[[1]], "theme") && is_class(x[[1]], "gg")
}

#' @name is_gg_component
#' @rdname is_gg
#' @export
#' @importFrom ggplot2 is.ggproto
is_gg_component <- function(x) {
  is.ggproto(x) || is_gg_scale(x) || is_gg_theme(x) || is_gg_sf_layer(x) || is_gg_layer(x)
}

#' @name is_gg_list
#' @rdname is_gg
#' @export
#' @importFrom ggplot2 is.ggplot
is_gg_list <- function(x) {
  all(sapply(x, ggplot2::is.ggplot))
}
