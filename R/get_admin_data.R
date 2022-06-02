#' @title Get U.S. State and County boundary data for a location
#'
#' @description Get U.S. states and counties using a very fast query.
#'
#' @param location A `sf`, `sfc`, or `bbox` object or a character string that
#'   matches a geoid, name, abb, or statefp for [us_states] or [us_counties].
#' @param class Class of data to return, "df" (default), "sf", "bbox", or "sfc"
#' @inheritParams st_bbox_ext
#' @param ... Additional parameters including geoid or name. For [get_states()],
#'   additional parameters can be abb or statefp.
#' @name get_admin_data
#' @example examples/get_admin_data.R
NULL

#' @rdname get_admin_data
#' @name get_states
#' @export
#' @importFrom rlang list2
#' @importFrom dplyr case_when
get_states <- function(location = NULL,
                       dist = NULL,
                       diag_ratio = NULL,
                       unit = NULL,
                       asp = NULL,
                       class = "df",
                       ...) {
  params <- rlang::list2(...)

  us_states <- us_states

  type <- ""

  if (is_sf(location, ext = TRUE)) {
    type <- "sf"
  }

  # FIXME: This could be simplified by using is_state_name and is_state_geoid
  if (type != "sf") {
    if (is_state_geoid(location)) {
      params$geoid <- location
      params$geoid <- match.arg(as.character(params$geoid), us_states$statefp)
    } else if (is_state_name(location)) {
      params$name <- location
      params$name <- match.arg(params$name, c(us_states$name, us_states$abb))
    }

    type <-
      dplyr::case_when(
        is_state_geoid(params$geoid) ~ "geoid",
        is_state_name(params$name) ~ "name"
      )
  }


  matching <-
    switch(type,
      "sf" = st_intersects_data(
        x = st_bbox_ext(
          x = location,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp
        ),
        data = us_states,
        crs = 3857
      ),
      "name" = (us_states$name %in% params$name) | (us_states$abb %in% params$name),
      "geoid" = (us_states$statefp %in% params$geoid)
    )

  df <- us_states[matching, ]

  admin_df_as_class(df, class = class)
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

  us_counties <- us_counties
  type <- ""

  if (is_sf(location, ext = TRUE)) {
    type <- "sf"
  }

  if (type != "sf") {
    if (is_county_geoid(location)) {
      params$geoid <- location
      params$geoid <- match.arg(as.character(params$geoid), us_counties$geoid)
    } else if (is_county_name(location)) {
      params$name <- location
      params$name <- match.arg(params$name, us_counties$name)
    }

    type <-
      dplyr::case_when(
        is_county_geoid(params$geoid) ~ "geoid",
        is_county_name(params$name) ~ "name"
      )
  }


  matching <-
    switch(type,
      "sf" = st_intersects_data(
        x = st_bbox_ext(
          x = location,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp
        ),
        data = us_counties,
        crs = 3857
      ),
      "name" = (us_counties$name %in% params$name),
      "geoid" = (us_counties$geoid %in% params$geoid)
    )

  df <- us_counties[matching, ]

  admin_df_as_class(df, class = class)
}


#' Spatial filter with st_intersects converting wkt data
#'
#' @noRd
#' @importFrom purrr map_lgl
#' @importFrom sf st_as_sfc st_intersects
st_intersects_data <- function(x, data, wkt = TRUE, crs = 3857) {
  x <- as_sfc(x, crs = crs)

  if (wkt && (crs == 3857)) {
    data <- sf::st_as_sfc(df$wkt, crs = 3857)
  }

  intersects <-
    purrr::map_lgl(
      data,
      ~ sf::st_intersects(.x, x, sparse = FALSE)
    )

  return(intersects)
}

#' @noRd
#' @importFrom sf st_as_sf st_as_sfc
#' @importFrom dplyr bind_cols
admin_df_as_class <- function(df, class = "df") {
  if (class == "df") {
    return(df)
  } else if (class == "bbox") {
    return(df$bbox)
  } else if (class == "sf") {
    df$bbox <- NULL
    wkt <- df$wkt
    df$wkt <- NULL

    return(
      sf::st_as_sf(
        dplyr::bind_cols(
          df,
          "geometry" = sf::st_as_sfc(wkt, crs = 3857)
        )
      )
    )
    # FIXME: The following should have worked but didn't
    # return(as_sf(df, from_crs = 3857))
  } else if (class == "sfc") {
    return(sf::st_as_sfc(df$wkt, crs = 3857))
  }
}
