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
#' @noRd
check_sf_list <- function(x) {
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

#' Get aspect ratio from string or for paper or page block
#'
#'
#' @param asp Aspect ratio of width to height as a numeric value (e.g. 0.33) or
#'   character (e.g. "1:3"). If numeric, get_asp returns the same value without modification.
#' @inheritParams get_paper
#' @inheritParams get_margin
#' @param block_asp If TRUE, get aspect ratio of the map/plot area (not including the margins); defaults to FALSE.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   get_asp("1:2")
#'
#'   get_asp(11 / 17)
#'
#'   get_asp(paper = "letter")
#' }
#' }
#' @rdname get_asp
#' @export
#' @importFrom stringr str_detect str_extract
#' @importFrom usethis ui_stop
get_asp <- function(asp = NULL,
                    paper = NULL,
                    orientation = NULL,
                    margin = NULL,
                    unit = NULL,
                    block_asp = FALSE) {

  if (is.null(paper)) {
    # Check aspect ratio
    if (is.character(asp) && stringr::str_detect(asp, ":")) {
      # If asp is provided as character string (e.g. "16:9") convert to a numeric ratio
      asp <- as.numeric(stringr::str_extract(asp, ".+(?=:)")) / as.numeric(stringr::str_extract(asp, "(?<=:).+"))
    } else if (!is.numeric(asp) && !is.null(asp)) {
      usethis::ui_stop("{usethis::ui_value('asp')} must be numeric (e.g. 0.666) or a string representing a width to height ratio (e.g. '4:6').")
    }
  } else if (!is.null(paper) && is.null(asp)) {
    # Get aspect ratio for text/plot/map block area
    paper <- get_paper(paper = paper, orientation = orientation)
    # TODO: Calculate difference between the two and then calculate the aspect ratio for the new area
    if (block_asp) {
      if (is.null(unit)) {
        unit <- paper$units
      }

      # Get margins and convert to numeric
      margin <- get_margin(margin = margin, paper = paper, orientation = orientation, unit = unit)
      margin <- as.numeric(margin)

      # Calculate width, height, and aspect ratio for text/plot/map block area
      plot_width <- paper$width - (margin[[2]] + margin[[4]])
      plot_height <- paper$height - (margin[[1]] + margin[[3]])
      asp <- plot_width / plot_height
    } else {
      asp <- paper$asp
    }
  }

  return(asp)
}
