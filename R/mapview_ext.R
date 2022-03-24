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

#' @rdname mapview_ext
#' @name mapview_exif
#' @inheritParams make_photo_popup
#' @inheritParams read_sf_exif
mapview_exif <- function(path = NULL,
                         filetype = "jpeg",
                         popup = TRUE) {
  photos <-
    read_sf_exif(
      path = NULL,
      filetype = NULL,
      bbox = NULL,
      sort = "lon",
      tags = NULL,
      ...
    )

  make_photo_popup(
    photos = photos,
    popup = popup
  )
}

#' @rdname mapview_ext
#' @name mapview_exif
#' @inheritParams get_flickr_photos
#' @inheritParams make_photo_popup
mapview_flickr <- function(x = NULL,
                           user_id = NULL,
                           tags = NULL,
                           api_key = NULL,
                           img_size = "s",
                           num = 25,
                           popup = TRUE,
                           ...) {
  photos <-
    get_flickr_photos(
      location = x,
      user_id = user_id,
      tags = tags,
      api_key = api_key,
      img_size = img_size,
      per_page = num,
      geometry = TRUE, # this is the default
      ...
    )

  make_photo_popup(
    photos = photos,
    popup = popup
  )
}

#' Make a photo popup
#'
#' Utility function for [mapview_flickr] and [mapview_exif]
#'
#' @param popup If `TRUE`, add a popup image to a leaflet map; defaults `TRUE`.
make_photo_popup <- function(photos,
                                popup = TRUE) {

  check_pkg_installed("leaflet")
  check_pkg_installed("leafpop")

  stopifnot(
    all(c("img_url", "img_width", "img_height") %in% names(photos))
  )

  leaflet_map <-
    leaflet::addCircleMarkers(
      leaflet::addTiles(leaflet::leaflet()),
      data = photos,
      group = "photos")

  img_url <- photos$img_url
  img_width <- photos$img_width
  img_height <- photos$img_height

  if (popup) {
    leaflet_map <-
      leafpop::addPopupImages(
        map = leaflet_map,
        image = img_url,
        width = img_width,
        height = img_height,
        group = "photos"
      )
  }

  return(leaflet_map)
}
