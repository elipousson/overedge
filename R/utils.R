#' @noRd
check_url <- function(x) {
  grepl(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
    )
}

#' @noRd
check_esri_url <- function(x) {
  grepl("/MapServer|/FeatureServer", x)
}

#' @aliases check_sf check_sf_list
#' @noRd
check_sf <- function(x) {
  is_sf <- ("sf" %in% class(x))

  if (!is_sf && is.list(x)) {
    all(vapply(x, check_sf, TRUE))
  } else {
    is_sf
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
