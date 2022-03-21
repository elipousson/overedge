#' Use FlickrAPI to get geotagged photos for a location
#'
#' Set API key using `FlickrAPI::set_flickr_api_key()` or pass to the api_key
#' parameter. Currently relies on fork of package at <https://github.com/elipousson/FlickrAPI>
#'
#' @param location A `sf` or `bbox` object to use in creating bounding box for
#'   getting photos from Flickr.
#' @inheritParams FlickrAPI::get_photo_search
#' @inheritParams st_bbox_ext
#' @param geometry If `TRUE`, convert data frame with information on photos to
#'   an `sf` object, Default: `TRUE`.
#' @param sort Supported options include "date-posted","date-taken",
#'   "interestingness", or "relevance"
#' @param desc If `TRUE` return images in descending sort order, if `FALSE`,
#'   return in ascending sort order. Ignored if sort is set to "relevance".
#' @param extras Defaults to "description", "date_taken", "tags", and "geo".
#' @param img_size Defaults to "s" (small). Options ranging from smallest to
#'   largest include "sq", "t", "s", "q", "m", "n", "z", "c", "l", and "o"
#' @param per_page Photos to return per page of search, Default: 100. Maximum
#'   250 if location is provided or 500 otherwise.
#' @param page If page is greater than length 1, the function uses
#'   `purrr::map_dfr()` to return results for all pages but this may cause
#'   issues with API access if a large page range is provided. Default: 1
#' @param orientation If img_size is length 1, photos are filtered to one or
#'   more of the supported orientations ("portrait", "landscape", and "square");
#'   defaults to `NULL`.
#' @param geometry If `TRUE`, include "geo" in extras and convert photos data
#'   frame to `sf` object.
#' @param crs Coordinate reference system of `sf` object to return if geometry
#'   is `TRUE`.
#' @return A data frame with photo information or `sf` object with geometry
#'   based on latitude and longitude of geocoded photos.
#' @seealso
#'  \code{\link[FlickrAPI]{getPhotoSearch}}
#' @rdname get_flickr_photos
#' @export
#' @importFrom purrr map_dfr
#' @importFrom dplyr rename mutate case_when filter
get_flickr_photos <- function(api_key = NULL,
                              location = NULL,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              user_id = NULL,
                              tags = NULL,
                              license_id = "cc0",
                              sort = "date-posted",
                              desc = TRUE,
                              img_size = "s",
                              extras = c("description", "date_taken", "tags", "geo"),
                              per_page = 100,
                              page = 1,
                              orientation = NULL,
                              geometry = TRUE,
                              crs = 4326) {
  check_pkg_installed(pkg = "FlickrAPI", repo = "koki25ando/FlickrAPI")

  flickr_crs <- 4326

  if (!is.null(location)) {
    # Get adjusted bounding box if any adjustment variables provided
    bbox <-
      st_bbox_ext(
        x = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = flickr_crs
      )
  } else {
    bbox <- NULL
  }

  if (geometry) {
    # Always include geo in the extras if geometry is TRUE
    extras <- unique(c(extras, "geo"))
  }

  if (!is.null(sort)) {
    if (desc) {
      dir <- "-desc"
    } else {
      dir <- "-asc"
    }

    sort <-
      match.arg(
        sort,
        c(
          paste0(c("date-posted", "date-taken", "interestingness"), dir),
          "relevance"
        )
      )
  }

  if (!is.null(img_size)) {
    img_size <-
      match.arg(
        img_size,
        c("sq", "t", "s", "q", "m", "n", "z", "c", "l", "o"),
        several.ok = TRUE
      )

    extras <-
      unique(
        c(
          extras,
          paste0("url_", img_size)
        )
      )
  }

  if (length(page) > 1) {
    purrr::map_dfr(
      page,
      ~ FlickrAPI::get_photo_search(
        api_key = api_key,
        user_id = user_id,
        tags = tags,
        licence_id = license_id,
        sort = sort,
        bbox = bbox,
        extras = extras,
        per_page = per_page,
        page = .x
      )
    )
  } else {
    photos <-
      FlickrAPI::get_photo_search(
        api_key = api_key,
        user_id = user_id,
        tags = tags,
        licence_id = license_id,
        sort = sort,
        bbox = bbox,
        extras = extras,
        per_page = per_page,
        page = page
      )
  }

  if (length(img_size) == 1) {
    image_url <- paste0("url_", img_size)
    image_height <- paste0("height_", img_size)
    image_width <- paste0("width_", img_size)

    photos <-
      dplyr::rename(
        photos,
        image_url = {{ image_url }},
        image_width = {{ image_width }},
        image_height = {{ image_height }}
      )

    if (!is.null(orientation)) {
      orientation <-
        match.arg(
          orientation,
          c("landscape", "portrait", "square"),
          several.ok = TRUE
        )

      photos <-
        dplyr::mutate(
          photos,
          img_orientation = dplyr::case_when(
            (image_width / image_height) > 1 ~ "landscape",
            (image_width / image_height) < 1 ~ "portrait",
            TRUE ~ "square"
          )
        )

      photos <-
        dplyr::filter(
          photos,
          img_orientation %in% orientation
        )
    }
  }

  if (geometry) {
    photos_sf <- df_to_sf(x = photos, coords = c("longitude", "latitude"), crs = crs)
    return(photos_sf)
  } else {
    return(photos)
  }
}
