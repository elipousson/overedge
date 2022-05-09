#' Create discrete fill and color scales for grouped data
#'
#' Designed for use with layer_group_data. group_data_pal generates palettes
#' that are passed to [ggplot2::scale_fill_manual] and
#' [ggplot2::scale_color_manual].
#'
#' @param data Data to use when generating scale or palette.
#' @param col Grouping column found in data to use in generating scale or
#'   palette; defaults to `NULL`.
#' @inheritParams paletteer::paletteer_d
#' @inheritParams ggplot2::scale_fill_manual
#' @inheritParams ggplot2::discrete_scale
#' @seealso
#'  [scales::viridis_pal()]
#'  [paletteer::paletteer_d()]
#' @name scale_group_data
#' @export
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
scale_group_data <-
  function(...,
           data,
           col = NULL,
           palette = NULL,
           n = NULL,
           direction = 1,
           na.value = "grey50",
           drop = FALSE,
           limits = NULL,
           aesthetics = "fill") {
    aesthetics <- match.arg(aesthetics, c("fill", "color"), several.ok = TRUE)

    group_pal <-
      group_data_pal(
        data = data,
        col = col,
        palette = palette,
        n = n,
        direction = direction
      )

    if (any(c("color", "fill") %in% aesthetics)) {
      manual_scale <-
        ggplot2::scale_fill_manual(
          ...,
          values = group_pal,
          limits = names(group_pal),
          na.value = na.value,
          drop = drop,
          aesthetics = aesthetics
        )

      discrete_scale <-
        ggplot2::scale_fill_discrete(
          limits = names(group_pal),
          type = manual_scale
        )

      return(discrete_scale)
    }
  }

#' @name group_data_pal
#' @rdname scale_group_data
#' @export
#' @importFrom dplyr n_groups filter mutate bind_cols group_keys
#' @importFrom tibble deframe
group_data_pal <- function(data,
                           palette = NULL,
                           col = NULL,
                           n = NULL,
                           direction = 1) {
  is_pkg_installed("scales")
  is_pkg_installed("paletteer")

  data <-
    group_by_col(
      data = data,
      col = col
    )

  if (is.null(n)) {
    n <- dplyr::n_groups(data)
  }

  if (is.null(palette)) {
    palette <- scales::viridis_pal()(n)
  } else {
    pal_opts <-
      dplyr::filter(
        paletteer::palettes_d_names,
        length >= n
      )

    pal_opts <-
      dplyr::mutate(
        pal_opts,
        pkg_pal = paste0(package, "::", palette)
      )

    palette <- match.arg(palette, pal_opts$pkg_pal)
    palette <- paletteer::paletteer_d(palette = palette, n = n, direction = direction)
  }

  group_palette <-
    tibble::deframe(
      dplyr::bind_cols(
        "values" = dplyr::group_keys(data),
        "colors" = palette
      )
    )

  return(group_palette)
}



#' @name get_group_data_pal_scale
#' @rdname scale_group_data
#' @export
get_group_data_pal_scale <-
  function(data, col = NULL, palette = NULL, ...) {
    group_palette <-
      group_data_pal(
        data = data,
        col = col,
        palette = palette,
        ...
      )

    group_scale <-
      scale_group_data(
        data = data,
        col = col,
        palette = palette,
        ...
      )

    return(list(
      "names" = names(group_palette),
      "palette" = group_palette,
      "scale" = group_scale
    ))
  }
