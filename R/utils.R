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

#' @noRd
check_class <- function(x, check = NULL) {
  any(check %in% class(x))
}

#' Check if an object is sf (or sf, sfc, or bbox) class
#'
#' @param x An object to check if it a `sf` class object or not.
#' @param ext If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or
#'   not; defaults to `FALSE`.
#' @noRd
check_sf <- function(x, ext = FALSE) {
  if (!ext) {
    check_class(x, "sf")
  } else {
    check_class(x, c("sf", "sfc", "bbox"))
  }
}

#' Check if an object is bbox class
#'
#' @param x An object to check if it a `bbox` class object or not.
#' @noRd
check_bbox <- function(x) {
  check_class(x, "bbox")
}


#' Check if object is a named list of sf class objects
#' @param x An object to check if it a named list of `sf` class objects or not.
#' @noRd
check_sf_list <- function(x) {
  is_sf_list <- is.list(x) && all(vapply(x, check_sf, TRUE))
  is_named <- !is.null(names(x)) && !("" %in% names(x))
  is_sf_list && is_named
}

#' Check if objects share the same coordinate reference system
#'
#' @param x An `sf`, `sfc`, or `bbox` object, or a character or numeric object
#'   supported by \code{\link[sf]{st_crs}}
#' @param y An object object to compare to x. The same object classes are
#'   supported.
#' @param transform If `TRUE`, transform y to match CRS of x and return Y. If
#'   `FALSE`, return logical indicator of whether x and y have the same crs;
#'   defaults to `FALSE`.
#' @noRd
check_sf_same_crs <- function(x, y) {
  return(sf::st_crs(x) == sf::st_crs(y))
}

#' Check if objects is a ggplot2 layer
#' @noRd
check_geom <- function(x) {
  check_class(x[[1]], "Layer") && check_class(x[[1]], "gg")
}

#' Check if package exists and prompt to install if not
#'
#' @param package Name of a package.
#' @importFrom checkmate test_directory_exists
#' @importFrom usethis ui_warn ui_yeah
#' @noRd
check_package_exists <- function(package) {
  if (!checkmate::test_directory_exists(find.package(package, quiet = TRUE))) {
    usethis::ui_warn("{usethis::ui_value(package)} is not installed.")
    if (usethis::ui_yeah("Do you want to try to install {package}?")) {
      install.packages(package)
    }
  }
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
