#' @importFrom checkmate test_character
#' @noRd
check_url <- function(x) {
  checkmate::test_character(
    x = x,
    pattern = "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  )
}

#' @importFrom checkmate test_character
#' @noRd
check_esri_url <- function(x) {
  checkmate::test_character(
    x = x,
    pattern = "/MapServer|/FeatureServer"
  )
}

#' @importFrom checkmate test_class
#' @aliases check_sf_list
#' @noRd
check_sf <- function(x) {
  if (!checkmate::test_class(x, "sf") && is.list(x)) {
    all(vapply(x, check_sf_list, TRUE))
  } else {
    checkmate::test_class(x, "sf")
  }
}

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
eval_data <- function(data, package = NULL, label = NULL) {
  data <- paste0(collapse = "::", c(package, data))
  eval(parse(text = data))
}

#' @importFrom sf st_transform st_coordinates
#' @noRd
st_coordinates_df <-
  function(x, crs = NULL) {
    if (!is.null(crs)) {
      x <- sf::st_transform(x, crs)
    }
    x <- subset(sf::st_coordinates(x), select = c(X, Y))
    as.data.frame(x)
  }
