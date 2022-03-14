#' Check if a string is a URL
#'
#' @noRd
check_url <- function(x) {
  grepl(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}
#' Check if a string is an ArcGIS MapServer or FeatureServer URL
#'
#' @noRd
check_esri_url <- function(x) {
  grepl("/MapServer|/FeatureServer", x)
}

#' Check if a string is a Google Sheets URL
#'
#' @noRd
check_gsheet_url <- function(x) {
  grepl(
    "^https://docs.google.com/spreadsheets/",
    x
  )
}

#' Check if objects is a ggplot2 layer
#' @noRd
check_geom <- function(x) {
  check_class(x[[1]], "Layer") && check_class(x[[1]], "gg")
}

#' Check if package exists and prompt to install if not
#'
#' @param package Name of a package.
#' @param repo GitHub repository to use for the package.
#' @importFrom rlang is_installed check_installed
#' @noRd
check_pkg_installed <- function(package, repo = NULL) {
  if (!rlang::is_installed(pkg = package)) {
    if (!is.null(repo)) {
      package <- repo
    }

    rlang::check_installed(pkg = package)
  }
}

modify_label_mapping <- function(mapping = NULL, modify = NULL, colname = NULL, data = NULL) {
  if (is.null(mapping)) {
    mapping <-
      ggplot2::aes()
  }

  if (!is.null(colname)) {
    if ("label" %in% modify) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(label = .data[[colname]]),
          mapping
        )
    } else if ("description" %in% modify) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(description = .data[[colname]]),
          mapping
        )
    }
  }

  if (("geometry" %in% modify) && !is.null(data)) {
    mapping <-
      utils::modifyList(
        ggplot2::aes(geometry = .data[[attributes(data)$sf_column]]),
        mapping
      )
  }

  return(mapping)
}

#' @noRd
eval_data <- function(data, package = NULL) {
  data <- paste0(collapse = "::", c(package, data))
  eval(parse(text = data))
}

#' @importFrom ggplot2 theme_set theme_update theme_replace
#' @noRd
theme_method <- function(x, method = NULL) {
  method <- match.arg(method, c("set", "update", "replace"))

  switch(method,
    "set" = ggplot2::theme_set(
      x
    ),
    "update" = ggplot2::theme_update(
      x
    ),
    "replace" = ggplot2::theme_replace(
      x
    )
  )
}

utils::globalVariables(c(
  ".data", ":=", "actual_unit", "asp_landscape", "asp_portrait",
  "coord_col", "img_orientation", "lat", "location_bbox", "lon",
  "map_icons", "name", "paper_sizes", "repo", "show_area",
  "standard_scales", "svg_url", "admin_level", "center", "gps_img_direction",
  "img_direction", "img_direction_ref", "latitude", "latitude_ref", "longitude",
  "longitude_ref", "orientation", "us_counties", "us_states",
  "gps_latitude", "gps_longitude", "osm_building_tags", "source_file"
))
