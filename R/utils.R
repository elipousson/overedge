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

#' Check if objects is a ggplot2 layer
#' @noRd
check_geom <- function(x) {
  check_class(x[[1]], "Layer") && check_class(x[[1]], "gg")
}

#' Check if package exists and prompt to install if not
#'
#' @param package Name of a package.
#' @param repo GitHub repository to use for the package.
#' @importFrom checkmate test_directory_exists
#' @importFrom usethis ui_warn ui_yeah
#' @importFrom remotes install_github
#' @noRd
check_package_exists <- function(package, repo = NULL) {
  if (!checkmate::test_directory_exists(find.package(package, quiet = TRUE))) {
    usethis::ui_warn("{usethis::ui_value(package)} is not installed.")
    if (usethis::ui_yeah("Do you want to try to install {package}?")) {

      if (is.null(repo)) {
        install.packages(package)
      } else {
        remotes::install_github(repo)
      }
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
