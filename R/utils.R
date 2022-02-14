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

#' @importFrom usethis ui_stop
#' @importFrom stringr str_detect str_extract
#' @noRd
get_asp <- function(asp) {
  # Check aspect ratio
  if (is.character(asp) && stringr::str_detect(asp, ":")) {
    # If asp is provided as character string (e.g. "16:9") convert to a numeric ratio
    as.numeric(stringr::str_extract(asp, ".+(?=:)")) / as.numeric(stringr::str_extract(asp, "(?<=:).+"))
  } else if (!is.numeric(asp) && !is.null(asp)) {
    usethis::ui_stop("`asp` must be numeric (e.g. 0.666) or a string with the ratio ratio of width to height (e.g. '4:6').")
  } else {
    asp
  }
}
