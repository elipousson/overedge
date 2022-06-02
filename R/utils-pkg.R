#' Set package options
#'
#' @param ... options to set, e.g. "crs = 2804" with `pkg = "overedge"` to set
#'   "overedge.crs" to 2804.
#' @param overwrite If `TRUE`, overwrite any existing option value.
#' @param pkg Name of a package; default to "overedge"
#' @param repo GitHub repository to use for the package.
#' @noRd
#' @importFrom rlang list2
#' @importFrom cli cli_alert_success cli_warn
set_pkg_options <- function(..., overwrite = TRUE, pkg = "overedge") {

  option <- rlang::list2(...)
  option_nm <- paste(pkg, names(option), sep = ".")
  existing_option <- getOption(option_nm)

  if (is.null(existing_option) | overwrite) {
    options(option_nm = option)
    cli::cli_alert_success(
      "{.var {option_nm}} set to {.val {as.character(option)}}."
    )
  } else if (!overwrite) {
    cli::cli_warn(
      "The option {.var {option_nm}} is {.val {existing_option}}.
    Set {.arg overwrite} to {.val TRUE} to replace with {.val {as.character(option)}}."
    )
  }
}

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

  if (geom %in% c("pattern")) {
    return(is_pkg_installed("ggpattern", repo = "coolbutuseless/ggpattern"))
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
