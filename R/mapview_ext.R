#' Use mapview to interactively explore spatial data
#'
#' A wrapper for [mapview::mapview]. x can be an sf object or sf list. If nm is
#' NULL and x is an sf list the first item in the list is used.
#'
#' @inheritParams mapview::mapview
#' @inheritDotParams mapview::mapview
#' @seealso
#'  [mapview::mapView()]
#' @name mapview_ext
NULL

#' @rdname mapview_ext
#' @name mapview_col
#' @param col Column name passed to zcol parameter, Default: `NULL`
#' @param nm Character vector. If x is a sf list, filter the mapview data to
#'   those named in nm. If `NULL`, mapview displays the first item in the sf
#'   list; defaults to "data".
#' @param na.rm If TRUE and col is not NULL, filter NA values from the col
#'   before passing to [mapview::mapview]
#' @inheritParams make_img_leafpop
#' @export
#' @importFrom dplyr filter
mapview_col <- function(x, col = NULL, nm = "data", na.rm = TRUE, ...) {
  is_pkg_installed(pkg = "mapview")

  if (is_sf_list(x)) {
    # FIXME: Add check for mixed geometry types
    if (!is.null(nm)) {
      nm_x <- (names(x) %in% nm)
      x <- x[nm_x]
    } else {
      x <- x[[1]]
    }
  }

  if (!is_sf(x)) {
    x <- as_sf(x)
  }

  if (!is.null(col)) {
    if (na.rm) {
      x <- dplyr::filter(x, !is.na(.data[[col]]))
    }

    mapview::mapview(x, zcol = col, ...)
  } else {
    mapview::mapview(x, ...)
  }
}

#' @rdname mapview_ext
#' @name mapview_exif
#' @inheritParams read_sf_exif
mapview_exif <- function(path = NULL,
                         filetype = "jpeg",
                         popup = TRUE,
                         ...) {
  photos <-
    read_sf_exif(
      path = path,
      filetype = filetype,
      ...
    )

  make_img_leafpop(
    images = photos,
    popup = popup
  )
}

#' @rdname mapview_ext
#' @name mapview_exif
#' @param num Number of images to display; defaults to 25. 250 is maximum using [get_flickr_photos()].
#' @inheritParams get_flickr_photos
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

  make_img_leafpop(
    images = photos,
    popup = popup
  )
}

#' @param popup If `TRUE`, add a popup image to a leaflet map; defaults `TRUE`.
#' @rdname mapview_ext
#' @param images A simple feature object with columns for the image path/url, image width, and image height.
#' @name make_img_leafpop
#' @export
make_img_leafpop <- function(images,
                             popup = TRUE) {
  is_pkg_installed("leaflet")
  is_pkg_installed("leafpop")

  stopifnot(
    all(c("img_width", "img_height") %in% names(images)),
    any(c("path", "img_url") %in% names(images)),
    is_sf(images)
  )

  leaflet_map <-
    leaflet::addCircleMarkers(
      leaflet::addTiles(leaflet::leaflet()),
      data = images,
      group = "images"
    )

  if ("img_url" %in% names(images)) {
    image <- images$img_url
  } else if ("path" %in% names(images)) {
    image <- images$path
  }

  width <- images$img_width
  height <- images$img_height

  if (popup) {
    leaflet_map <-
      leafpop::addPopupImages(
        map = leaflet_map,
        image = image,
        width = width,
        height = height,
        group = "images"
      )
  }

  return(leaflet_map)
}
