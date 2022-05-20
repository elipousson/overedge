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

  us_states <- overedge::us_states

  # FIXME: This could be simplified by using is_state_name and is_state_geoid
  if (is.character(location)) {
    if (location %in% us_states$geoid) {
      params$geoid <- location
    } else if (location %in% us_states$abb) {
      params$abb <- location
    } else if (location %in% us_states$statefp) {
      params$statefp <- location
    } else if (location %in% us_states$name) {
      params$name <- location
    }
  }

  type <-
    dplyr::case_when(
      is_sf(location, ext = TRUE) ~ "sf",
      !is.null(params$geoid) ~ "geoid",
      !is.null(params$abb) ~ "abb",
      !is.null(params$statefp) ~ "statefp",
      !is.null(params$name) ~ "name"
    )

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
      "abb" = (us_states$abb %in% match.arg(params$abb, us_states$abb)),
      "name" = (us_states$name %in% match.arg(params$name, us_states$name)),
      "statefp" = (us_states$statefp %in% match.arg(params$statefp, us_states$statefp)),
      "geoid" = (us_states$geoid %in% match.arg(params$geoid, us_states$geoid))
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

  us_counties <- overedge::us_counties

  if (is.character(location)) {
    if (is_county_geoid(location)) {
      params$geoid <- location
    } else if (is_county_name(location)) {
      params$name <- location
    }
  }

  type <-
    dplyr::case_when(
      is_sf(location, ext = TRUE) ~ "sf",
      !is.null(params$geoid) ~ "geoid",
      !is.null(params$name) ~ "name"
    )

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
      "name" = (us_counties$name %in% match.arg(params$name, us_counties$name)),
      "geoid" = (us_counties$geoid %in% match.arg(params$geoid, us_counties$geoid))
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
