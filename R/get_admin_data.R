#' @title Get U.S. State and County boundary data for a location
#'
#' @description Get U.S. states and counties using a very fast query.
#'
#' @param location sf, sfc, or bbox object convertible with as_sfc
#' @param class Class of data to return, "df" (default), "sf", "bbox", or "sfc"
#' @inheritParams st_bbox_ext
#' @param ... Additional parameters including GeoID
#' @name get_admin_data
#' @importFrom purrr map_lgl
#' @importFrom sf st_as_sfc st_intersects
NULL

#' @rdname get_admin_data
#' @name get_states
#' @export
get_states <- function(location = NULL,
                       dist = NULL,
                       diag_ratio = NULL,
                       unit = NULL,
                       asp = NULL,
                       class = "df",
                       ...) {
  params <- rlang::list2(...)

  us_states <- overedge::us_states

  if (!is.null(location)) {
    # Adjust bounding box
    bbox <- st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp
    )

    matching <-
      st_intersects_data(x = bbox, data = us_states, crs = 3857)
  } else if (!any(c(params$abb, params$name, params$statefp, params$geoid), is.null)) {
    if (!is.null(params$abb)) {
      params$abb <- match.arg(params$abb, us_states$abb)
      matching <- (us_states$abb %in% params$abb)
    } else if (!is.null(params$name)) {
      params$name <- match.arg(params$name, us_states$name)
      matching <- (us_states$name %in% params$name)
    } else if (!is.null(params$statefp)) {
      params$statefp <- match.arg(params$statefp, us_states$statefp)
      matching <- (us_states$statefp %in% params$statefp)
    } else if (!is.null(params$geoid)) {
      params$geoid <- match.arg(params$geoid, us_states$geoid)
      matching <- (us_states$geoid %in% params$geoid)
    }
  }

  df <- us_states[matching, ]

  if (class == "df") {
    return(df)
  } else if (class == "bbox") {
    return(df$bbox)
  } else if (class == "sf") {
    df$geometry <- as_sfc(df$wkt)
    return(sf::st_as_sf(df, crs = 3857))
  } else if (class == "sfc") {
    return(as_sfc(df$wkt))
  }
}

#' @rdname get_admin_data
#' @name get_counties
#' @export
get_counties <- function(location = NULL,
                         dist = NULL,
                         diag_ratio = NULL,
                         unit = NULL,
                         asp = NULL,
                         class = "df",
                         ...) {
  params <- rlang::list2(...)

  us_counties <- overedge::us_counties

  if (!is.null(location)) {
    # Adjust bounding box
    bbox <- st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp
    )

    matching <-
      st_intersects_data(x = bbox, data = us_counties, crs = 3857)
  } else if (!any(sapply(c(params$geoid), is.null))) {
    if (!is.null(params$geoid)) {
      params$geoid <- match.arg(params$geoid, us_counties$geoid)
      matching <- (us_counties$geoid %in% params$geoid)
    }
  }

  df <- us_counties[matching, ]

  if (class == "df") {
    return(df)
  } else if (class == "bbox") {
    return(df$bbox)
  } else if (class == "sf") {
    df$geometry <- as_sfc(df$wkt)
    return(sf::st_as_sf(df, crs = 3857))
  } else if (class == "sfc") {
    return(as_sfc(df$wkt))
  }
}

#' Spatial filter with st_intersects converting wkt data
#'
#' @noRd
st_intersects_data <- function(x, data, wkt = TRUE, crs = 3857) {
  x <- as_sfc(x, crs = crs)

  if (wkt && (crs == 3857)) {
    data <- as_sfc(data$wkt)
  }

  intersects <-
    purrr::map_lgl(
      data,
      ~ sf::st_intersects(.x, x, sparse = FALSE)
    )

  return(intersects)
}
