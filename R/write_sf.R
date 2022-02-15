#' @title Save simple features to files, cache features, or load features to the environment
#'
#' This function wraps `sf::write_sf()` and `list2env()` to facilitate both
#' saving simple feature objects and named lists of simple feature objects.
#'
#' @param x sf object or a list of sf objects
#' @param filetype File type to write and cache, Default: 'geojson'
#' @param data_dir Data directory to write files, Default: NULL
#' @param save If TRUE, write the ; defaults to TRUE
#' @param load If TRUE, load sf objects to global environment; defaults to FALSE
#' @param cache If TRUE, write sf object(s) to file in cache directory; defaults to FALSE
#' @seealso
#'  \code{\link[sf]{st_write}}
#' @rdname write_sf_ext
#' @export
#' @importFrom purrr discard walk
#' @importFrom sf write_sf
#' @importFrom glue glue
write_sf_ext <- function(
    data,
    filename = NULL,
    filetype = "geojson",
    data_dir = NULL,
    save = FALSE,
    load = TRUE,
    cache = FALSE) {


  if (!is.list(data)) {
    name <- deparse(substitute(data))
    data <- list(data)
    names(data) <- name
  }

  data <- purrr::discard(data, ~ nrow(.x) == 0)

  if (!check_sf_list(data)) {
    usethis::ui_stop("write_sf_ext works with sf objects and lists containing sf objects. Mixed class lists or non-sf objects are not supported.")
  }

  if (save) {
    purrr::walk(
      data,
      ~ sf::write_sf(
        obj = .x,
        dsn = file.path(data_dir, glue::glue("{names(.x)}.{filetype}")))
    )
  }

  if (load) {
    list2env(data, envir = globalenv())
  }

  if (cache) {
    purrr::walk(
      data,
      ~ cache_sf(
        data = .x,
        filename = glue::glue("{names(.x)}.{filetype}"))
    )
  }
}


cache_sf <- function(data,
                     label = NULL,
                     filename = NULL,
                     filetype = "gpkg",
                     date = FALSE,
                     auto) {

}
