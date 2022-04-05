#' Is this package installed?
#'
#' @param package Name of a package.
#' @param repo GitHub repository to use for the package.
#' @importFrom rlang is_installed check_installed
#' @noRd
is_pkg_installed <- function(pkg, repo = NULL) {
  if (!rlang::is_installed(pkg = pkg)) {
    if (!is.null(repo)) {
      pkg <- repo
    }

    rlang::check_installed(pkg = pkg)
  }
}

#' Is the package needed for this geom installed?
#'
#' @noRd
is_geom_pkg_installed <- function(geom) {

  # Check if packages are available for text/label geoms
  if (geom %in% c("textsf", "labelsf")) {
    return(is_pkg_installed("geomtextpath"))
  }

  # Check if packages are available for text/label geoms
  if (geom %in% c("mark", "mapbox", "location", "context", "markers", "numbered")) {
    return(is_pkg_installed(pkg = "birdseyeview", repo = "elipousson/birdseyeview"))
  }


  if (geom %in% c("text_repel", "label_repel")) {
    return(is_pkg_installed("ggrepel"))
  }
}

#' @noRd
ls_pkg_data <- function(pkg, envir = .GlobalEnv) {
  utils::data(package = pkg, envir = envir)$results[, "Item"]
}

#' @noRd
ls_pkg_extdata <- function(pkg) {
  list.files(system.file("extdata", package = pkg))
}

#' @noRd
ls_pkg_cache <- function(pkg) {
  list.files(get_data_dir(path = NULL, package = pkg))
}
