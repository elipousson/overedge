.onLoad <- function(lib, pkg) {
  run_on_load()

  utils::data(
    list = c(
      "us_counties", "us_states",
      "paper_sizes", "standard_scales",
      "map_icons", "osm_building_tags",
      "dist_unit_options", "dist_units",
      "area_unit_options"
    ),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(c(
  ".data", ":=", "actual_unit",
  "coord_col", "img_orientation", "lat", "location_bbox", "lon",
  "name", "repo", "show_area",
  "svg_url", "admin_level", "center", "gps_img_direction",
  "img_direction", "img_direction_ref", "latitude", "latitude_ref", "longitude",
  "longitude_ref", "orientation",
  "gps_latitude", "gps_longitude", "source_file",
  "address", "asp", "block_height", "block_width", "col_width", "cols", "geometry", "gutter",
  "height", "id", "image_description", "image_height", "image_width",
  "row_height", "rows", "width", "df", "where", ".id", "nm",
  "angle", "number_col", "package", "paper", "x", "x1", "x2", "y1", "y2",
  "get_start_end_point", "path"
))

#' Group data by column if present
#'
#' @param data Data frame or simple feature object
#' @param col Column name/value
#' @noRd
group_by_col <- function(data, col = NULL) {
  if (is.null(col) || is.null(data)) {
    return(data)
  }

  if ((rlang::has_length(col, 1)) && rlang::has_name(data, col)) {
    return(dplyr::group_by(data, .data[[col]]))
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
    # TODO: Substitute dplyr::bind_cols() instead
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

    if (("color" %in% names(params)) && !is.null(params$color)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(color = .data[[params$color]]),
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

#' Modify function parameters
#'
#' @noRd
modify_fn_fmls <- function(params, fn, keep_missing = FALSE, keep.null = FALSE, ...) {
  fmls <- rlang::fn_fmls(fn)

  if (!keep_missing) {
    fmls <- purrr::discard(fmls, rlang::is_missing)
  }

  params <- c(rlang::list2(...), params)

  utils::modifyList(
    fmls,
    params,
    keep.null = keep.null
  )
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
  if (is.null(fn)) {
    return(data)
  }

  fn <- rlang::as_function(fn)
  fn(data)
}

#' Does the data frame has a column with the same name?
#'
#' @name has_same_name_col
#' @noRd
#' @importFrom rlang has_name
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom dplyr rename
has_same_name_col <- function(x, col = NULL, prefix = "orig", ask = FALSE, quiet = FALSE, drop = TRUE) {
  if (rlang::has_name(x, col) && !drop) {
    new_col <- paste0(prefix, "_", col)

    if (ask && !quiet) {
      if (!cli_yeah("The provided data includes an existing column named '{col}'.
                   Do you want to proceed and rename this column to {new_col}?")) {
        cli::cli_abort("Please rename your column to use this function.")
      }
    }

    if (!quiet) {
      cli::cli_alert_success(
        "The existing column '{col}' to '{new_col}' to avoid overwriting any existing values."
      )
    }

    x <-
      dplyr::rename(
        x,
        "{new_col}" := col
      )
  } else if (rlang::has_name(x, col) && drop) {
    x <-
      dplyr::select(
        x,
        -dplyr::all_of(col)
      )
  }

  x
}

#' Set join function based on geometry type
#'
#' @name set_join_by_geom_type
#' @inheritParams is_geom_type
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects] if key_list contains only POLYGON or MULTIPOLYGON
#'   objects or [sf::st_nearest_feature] if key_list contains other types.
#' @importFrom sf st_intersects st_nearest_feature
#' @noRd
set_join_by_geom_type <- function(x, join = NULL) {
  if (!is.null(join)) {
    return(join)
  }

  if (all(sapply(x, is_polygon) | sapply(x, is_multipolygon))) {
    return(sf::st_intersects)
  }

  sf::st_nearest_feature
}
