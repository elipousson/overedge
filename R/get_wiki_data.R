#' Get Wikipedia articles for a location
#'
#' Use the Wikipedia API geosearch API to get Wikipedia articles for a location.
#' See <https://www.mediawiki.org/wiki/Extension:GeoData> for more information.
#' Only returns Wikipedia articles with coordinates.
#'
#' @inheritParams st_bbox_ext
#' @inheritParams get_location_data
#' @param radius If TRUE, use dist as a buffer around the center of the
#'   location.
#' @param primary If TRUE, search for primary coordinates. Set primary to "all"
#'   or "secondary" to search other coordinate types.
#' @param details Additional detailed to return with results. Options include
#'   "type", "name", "country", "region"
#' @param limit Number of pages to return (max 500); deafaults to 50
#' @param lang Language to search on Wikipedia; defaults to "en".
#' @param geometry If TRUE, return sf object. If FALSE, return dataframe. Defaults to FALSE.
#' @rdname get_wiki_data
#' @export
#' @importFrom httr2 request req_url_query req_perform resp_body_json
get_wiki_data <- function(location,
                          dist = 100,
                          diag_ratio = NULL,
                          asp = NULL,
                          unit = "meter",
                          radius = FALSE,
                          primary = NULL,
                          details = c("type", "name", "region"),
                          lang = "en",
                          limit = 50,
                          geometry = TRUE) {

  # <https://www.mediawiki.org/wiki/Extension:GeoData>
  req <- httr2::request(paste0("https://", lang, ".wikipedia.org/w/api.php"))
  req <-
    httr2::req_url_query(
      req,
      action = "query",
      list = "geosearch"
    )

  stopifnot(
    !is.null(location)
  )

  details <-
    match.arg(details, c("type", "name", "country", "region"), several.ok = TRUE)

  if (limit > 500) {
    limit <- 500
  }

  if (!radius) {
    req <-
      httr2::req_url_query(
        req,
        gsbbox = make_gsbbox(location, dist = dist, diag_ratio = diag_ratio, asp = asp, unit = unit)
      )
  } else {
    #  dist <- as.numeric(sf_bbox_diagdist(bbox, drop = FALSE)) / 2
    req <-
      httr2::req_url_query(
        req,
        gscoord = make_gscoord(location),
        gsradius = dist
      )
  }

  if (!is.null(primary)) {
    primary <- match.arg(primary, c("all", "primary", "secondary"))

    req <-
      httr2::req_url_query(
        req,
        gsprimary = paste0(primary, collapse = "|")
      )
  }

  req <-
    httr2::req_url_query(
      req,
      gslimit = limit,
      format = "json"
    )

  resp <-
    httr2::resp_body_json(
      resp = httr2::req_perform(req = req),
      simplifyVector = TRUE
    )

  if (rlang::has_name(resp, "error")) {
    cli::cli_abort(resp$error$info)
  }

  if (geometry) {

    # return(resp$query$geosearch)
    return(df_to_sf(resp$query$geosearch))
  }

  return(resp$query$geosearch)
}

#' Make geospatial coordinate query
#'
#' @noRd
make_gscoord <- function(location, crs = 4326) {
  # center <- st_center(location, ext = FALSE)
  center <- get_coords(location, crs = crs)
  paste0(center$lat, "|", center$lon)
}

#' Make geospatial bbox query
#'
#' @noRd
make_gsbbox <- function(location, dist = NULL, diag_ratio = NULL, asp = NULL, unit = "meter", crs = 4326) {
  bbox <-
    st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      asp = asp,
      unit = unit,
      crs = crs
    )

  # top|left|bottom|right order
  paste0(
    c(
      bbox["ymax"],
      bbox["xmin"],
      bbox["ymin"],
      bbox["xmax"]
    ),
    collapse = "|"
  )
}
