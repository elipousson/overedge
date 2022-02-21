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

#' @noRd
check_sf <- function(x) {
  "sf" %in% class(x)
}

#' @noRd
check_sf_list <- function(x) {
  is_sf_list <- is.list(x) && all(vapply(x, check_sf, TRUE))
  is_named <- !is.null(names(x)) && !("" %in% names(x))
  is_sf_list && is_named
}

#' @noRd
check_bbox <- function(x) {
  "bbox" %in% class(x)
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
