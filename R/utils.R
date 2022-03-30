#' Is this a URL?
#'
#' @noRd
is_url <- function(x) {
  grepl(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}

#' Is this an ArcGIS MapServer or FeatureServer URL?
#'
#' @noRd
is_esri_url <- function(x) {
  grepl("/MapServer|/FeatureServer", x)
}

#' Is this a Google Sheets URL?
#'
#' @noRd
is_gsheet <- function(x) {
  grepl(
    "^https://docs.google.com/spreadsheets/",
    x
  )
}

as_gsheet_ss <- function() {

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

#' @noRd
#' @importFrom usethis ui_todo
ui_ask <- function(x) {
  readline(prompt = usethis::ui_todo(x))
}

#' Group data by column if present
#'
#' @param data Data frame or simple feature object
#' @param col Column name/value
#' @noRd
group_by_col <- function(data, col = NULL) {
  if (!is.null(col) && (rlang::has_length(col, 1)) && !is.null(data)) {
    if (rlang::has_name(data, col)) {
      dplyr::group_by(data, .data[[col]])
    }
  } else {
    data
  }
}

#' Add column to data if not present
#'
#' @param data Data frame or simple feature object
#' @param col Column name/value
#' @noRd
add_col <- function(data, col = NULL) {
  if (!is.null(col) && !(col %in% names(data)) && any(length(col) %in% c(nrow(data), 1))) {
    # FIXME: This is a non-standard pattern - I like it but it may or may not be appropriate and should be documented
    data[[col]] <- col
  }

  data
}

#' Modify mapping for ggplot2 aesthetics
#'
#' @param mapping aesthetic mapping to modify
#' @param data Data used to determine sf column for geometry aesthetic
#' @param ... Additional parameters with aesthetics to modify and column values
#'   to use, e.g. label = label_col
#' @noRd
modify_mapping <- function(mapping = NULL, data = NULL, ...) {
  if (is.null(mapping)) {
    mapping <-
      ggplot2::aes()
  }

  params <- rlang::list2(...)

  if (!is.null(params)) {
    if (("label" %in% names(params)) && !is.null(params$label)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(label = .data[[params$label]]),
          mapping
        )
    }

    if (("description" %in% names(params)) && !is.null(params$description)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(description = .data[[params$description]]),
          mapping
        )
    }

    if (("fill" %in% names(params)) && !is.null(params$fill)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(fill = .data[[params$fill]]),
          mapping
        )
    }

    if (("size" %in% names(params)) && !is.null(params$size)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(size = .data[[params$size]]),
          mapping
        )
    }

    if (("color" %in% names(params)) && !is.null(params$color)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(color = .data[[params$color]]),
          mapping
        )
    }

    if (("linetype" %in% names(params)) && !is.null(params$linetype)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(linetype = .data[[params$linetype]]),
          mapping
        )
    }
  }

  if (!is.null(data)) {
    mapping <-
      utils::modifyList(
        ggplot2::aes(geometry = .data[[attributes(data)$sf_column]]),
        mapping
      )
  }

  return(mapping)
}

#' Eval and parse data
#'
#' @noRd
use_eval_parse <- function(data, package = NULL) {
  data <- paste0(collapse = "::", c(package, data))
  eval(parse(text = data))
}

#' Apply function to data
#'
#' @param data Data to apply function to
#' @param fn defaults to NULL
#' @importFrom rlang as_function
#' @noRd
use_fn <- function(data, fn = NULL) {
  if (!is.null(fn)) {
    fn <- rlang::as_function(fn)
    data <- fn(data)
  }

  return(data)
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


#' Modified version of [usethis::ui_yeah]
#'
#' @noRd
#' @importFrom glue glue_collapse glue
#' @importFrom rlang is_interactive
#' @importFrom cli cli_abort cli_alert
#' @importFrom utils menu
cli_yeah <- function(x,
                     yes = c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely"),
                     no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
                     n_yes = 1,
                     n_no = 2,
                     shuffle = TRUE,
                     .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)

  if (!rlang::is_interactive()) {
    cli::cli_abort(
      c(
        "User input required, but session is not interactive.",
        "Query: {x}"
      )
    )
  }

  n_yes <- min(n_yes, length(yes))
  n_no <- min(n_no, length(no))
  qs <- c(sample(yes, n_yes), sample(no, n_no))

  if (shuffle) {
    qs <- sample(qs)
  }

  cli::cli_alert(x)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}

#' @noRd
#' @importFrom cli cli_alert
cli_ask <- function(x, prompt = ">> ") {
  cli::cli_alert(x)
  readline(prompt = prompt)
}


utils::globalVariables(c(
  ".data", ":=", "actual_unit",
  "coord_col", "img_orientation", "lat", "location_bbox", "lon",
  "map_icons", "name", "paper_sizes", "repo", "show_area",
  "standard_scales", "svg_url", "admin_level", "center", "gps_img_direction",
  "img_direction", "img_direction_ref", "latitude", "latitude_ref", "longitude",
  "longitude_ref", "orientation", "us_counties", "us_states",
  "gps_latitude", "gps_longitude", "osm_building_tags", "source_file",
  "address", "asp", "block_height", "block_width", "col_width", "cols", "geometry", "gutter",
  "height", "id", "image_description", "image_height", "image_width",
  "row_height", "rows", "width", "df", "where"
))
