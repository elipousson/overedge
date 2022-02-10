#' @importFrom sf st_transform st_coordinates
get_coord_df <-
  function(x, crs = NULL) {
    if (!is.null(crs)) {
     x <- sf::st_transform(x, crs)
    }
    x <- subset(sf::st_coordinates(x), select = c(X, Y))
    as.data.frame(x)
  }

#' @importFrom usethis ui_stop
#' @importFrom stringr str_detect str_extract
get_asp <- function(asp) {
  # Check aspect ratio
  if (is.character(asp) && stringr::str_detect(asp, ":")) {
    # If asp is provided as character string (e.g. "16:9") convert to a numeric ratio
    as.numeric(stringr::str_extract(asp, ".+(?=:)")) / as.numeric(stringr::str_extract(asp, "(?<=:).+"))
  } else if (!is.numeric(asp) && !is.null(asp)) {
    usethis::ui_stop("`asp` must be numeric (e.g. 0.666) or a string with the ratio ratio of width to height (e.g. '4:6').")
  } else {
    asp
  }
}

#' Convert a bounding box into simple feature object
#'
#' @param bbox bbox object
#' @return sf object
#' @seealso
#'  - \code{\link[sf]{st_as_sf}},\code{\link[sf]{st_as_sfc}}
#'  - \code{\link[sfx]{st_extent}}
#' @rdname sf_bbox_to_sf
#' @export
#' @importFrom sf st_as_sf st_as_sfc
sf_bbox_to_sf <- function(bbox) {
  sf::st_as_sf(sf::st_as_sfc(bbox))
}
