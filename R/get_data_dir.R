
#' Check data directory and create if needed
#'
#' A utility function that wraps [rappdirs::user_cache_dir].
#'
#' @param path Path to directory to use as data directory.
#' @param package Package name; defaults to "overedge"
#' @export
#' @importFrom rappdirs user_cache_dir
#' @importFrom checkmate test_directory_exists
#' @importFrom usethis ui_yeah ui_done
get_data_dir <- function(path = NULL, package = "overedge") {
  if (is.null(path)) {
    path <- rappdirs::user_cache_dir(package)
  }

  if (!checkmate::test_directory_exists(path)) {
    if (cli_yeah(
      "The directory {.file {path}} does not exist.
    Do you want to create a directory at this location?"
    )) {
      dir.create(path = path)
      cli::cli_alert_success("New directory created at {.file {path}}")
    } else {
      cli::cli_abort("Please provide a different path for this file.")
    }
  }

  path
}
