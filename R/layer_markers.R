
#' Create a ggplot2 layer with map markers or numbered markers
#'
#' If get is `TRUE`, groupname_col, group_meta, crs, and fn is all passed on to
#' make_markers.
#'
#' @inheritParams make_markers
#' @param make If `TRUE`, pass data to [make_markers].
#' @param number If `TRUE`, number markers using [layer_markers()] (not
#'   currently supported)
#' @inheritParams number_features
#' @param style Style; defaults to `NULL` for [layer_markers()] (supports
#'   "facet"); defaults to "roundrect" for [layer_markers()] when numbered =
#'   TRUE,
#' @param ... Additional parameters passed to [layer_group_data()]
#' @return ggplot2 layers
#' @example examples/layer_markers.R
#' @name layer_markers
#' @family layer
#' @md
#' @export
layer_markers <- function(data,
                          mapping = NULL,
                          geom = "sf",
                          make = FALSE,
                          groupname_col = NULL,
                          group_meta = NULL,
                          crs = NULL,
                          number = FALSE,
                          num_by_group = FALSE,
                          num_style = NULL,
                          num_start = 1,
                          suffix = NULL,
                          sort = "dist_xmin_ymax",
                          desc = FALSE,
                          fn = NULL,
                          ...) {
  if (make) {
    data <-
      make_markers(
        data = data,
        groupname_col = groupname_col,
        group_meta = group_meta,
        crs = crs,
        fn = fn
      )
  }

  if (number) {
    col <- NULL
    if (num_by_group) {
      col <- groupname_col
    }

    data <-
      number_features(
        data = data,
        col = col,
        sort = sort,
        desc = desc,
        num_style = num_style,
        num_start = num_start,
        suffix = suffix
      )

    mapping <-
      modify_mapping(
        mapping = mapping,
        label = "number"
      )
  }

  if (!is.null(groupname_col)) {
    if (is_point(data) || is_multipoint(data)) {
      mapping <-
        modify_mapping(
          mapping = mapping,
          color = groupname_col
        )
    } else {
      mapping <-
        modify_mapping(
          mapping = mapping,
          fill = groupname_col
        )
    }
  }

  layer_location_data(
    data = data,
    geom = geom,
    mapping = mapping,
    ...
  )
}

#' @rdname layer_markers
#' @name layer_numbers
#' @param style Style of number markers to map; defaults to "roundrect".
#' @param size Marker size, Default: 5
#' @param num_by_group If `TRUE`, numbers are added by group based on
#'   groupname_col.
#' @inheritParams number_features
#' @inheritParams ggplot2::geom_sf_label
#' @param hjust,vjust Horizontal and vertical justification.
#' @param ... Additional parameters passed to [layer_location_data()]
#' @export
#' @importFrom ggplot2 aes unit
#' @importFrom rlang list2
#' @importFrom purrr list_modify zap
#' @importFrom utils modifyList
#' @importFrom dplyr arrange mutate row_number
layer_numbers <- function(data,
                          mapping = NULL,
                          geom = "label",
                          make = FALSE,
                          groupname_col = NULL,
                          style = "roundrect",
                          size = 5,
                          sort = "dist_xmin_ymax",
                          num_by_group = FALSE,
                          num_style = NULL,
                          num_start = 1,
                          suffix = NULL,
                          desc = FALSE,
                          fn = NULL,
                          crs = NULL,
                          label.size = 0.0,
                          label.padding = ggplot2::unit(size / 10, "lines"),
                          label.r = label.padding * 1.5,
                          hjust = 0.5,
                          vjust = 0.5,
                          ...) {
  if ("roundrect" %in% style) {
    label.size <- 0.0
    label.padding <- ggplot2::unit(size / 10, "lines")
    label.r <- label.padding * 1.5
    hjust <- 0.5
    vjust <- 0.5
  }

  layer_markers(
    data = data,
    mapping = mapping,
    number = TRUE,
    geom = geom,
    sort = sort,
    num_style = num_style,
    suffix = suffix,
    num_by_group = num_by_group,
    size = size,
    label.size = label.size,
    label.padding = label.padding,
    label.r = label.r,
    hjust = hjust,
    vjust = vjust,
    ...
  )
}
