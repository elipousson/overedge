#' Set overedge packge options
#'
#' Can set options for package, diag_ratio, dist, asp, or crs
#'
#' @param dist,diag_ratio,unit,asp,data_package,data_filetype,from_crs,crs options to set, e.g. "crs = 2804" with
#'   `pkg = "overedge"` to set "overedge.crs" to 2804.
#' @param overwrite If `TRUE`, overwrite any existing option value.
#' @export
#' @importFrom rlang list2
#' @importFrom cli cli_alert_success cli_warn cli_vec
set_overedge_options <- function(dist = NULL,
                                 diag_ratio = NULL,
                                 unit = NULL,
                                 asp = NULL,
                                 data_package = NULL,
                                 data_filetype = NULL,
                                 from_crs = NULL,
                                 crs = NULL,
                                 overwrite = TRUE) {
  possible_options <-
    c(
      "overedge.dist",
      "overedge.diag_ratio",
      "overedge.unit",
      "overedge.asp",
      "overedge.data_package",
      "overedge.data_filetype",
      "overedge.from_crs",
      "overedge.crs"
    )

  update_options <-
    purrr::set_names(
      list(
        dist, diag_ratio, unit, asp,
        data_package, data_filetype, from_crs,
        crs
      ),
      nm = possible_options
    )

  update_options <-
    update_options[!sapply(update_options, is.null)]

  existing_options <-
    sapply(
      possible_options,
      getOption
    )

  existing_options <-
    existing_options[!sapply(existing_options, is.null)]


  if (overwrite | all(sapply(existing_options, is.null))) {
    options(
      update_options
    )

    update_options <-
      cli::cli_vec(update_options, style = list(vec_last = " and "))

    cli::cli_alert_success(
      "overedge options updated for {.arg {update_options}}."
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
