#' Use mapview to interactively explore data
#'
#' A wrapper for [mapview::mapview]
#'
#' @inheritParams mapview::mapview
#' @inheritDotParams mapview::mapview
#' @seealso
#'  \code{\link[mapview]{mapView}}
#' @name mapview_ext
NULL

#' @rdname mapview_ext
#' @name mapview_col
#' @param nm Character vector. If x is a sf list, filter the mapview data to
#'   those named in nm. If NULL, mapview displays the first item in the sf list;
#'   defaults to "data".
#' @param col Column name passed to zcol parameter, Default: NULL
#' @export
#' @importFrom mapview mapview
mapview_col <- function(x, nm = "data", col = NULL, ...) {
  check_pkg_installed(pkg = "mapview")

  if (is_sf(x, ext = TRUE)) {
    x <- as_sf(x)

    # FIXME: Add check for mixed geometry types
  } else if (is_sf_list(x)) {
    if (!is.null(nm)) {
      nm_x <- (names(x) %in% nm)
      x <- x[nm_x]
    } else {
      # TODO: Document pattern of using the first item is the list if nm is NULL
      x <- x[[1]]
    }
  }

  if (!is.null(col)) {
    mapview::mapview(x, zcol = col, ...)
  } else {
    mapview::mapview(x, ...)
  }
}
