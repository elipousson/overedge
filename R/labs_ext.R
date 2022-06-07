#' Add labels to a ggplot2 location map
#'
#' A helper function that converts strings to glue strings for the title,
#' subtitle, and caption. In progress.
#'
#' @inheritParams ggplot2::labs
#' @param location sf or bbox object or character string
#' @param name_col Column name holding name or identifier for distinct
#'   places within the simple feature collection provided to location. Not
#'   supported for bbox objects.
#' @param source Data source(s). Not yet used or supported by function.
#' @param alt Text used for the generation of alt-text for the plot.
#' @param .na,.null Additional parameters passed to [glue::glue]
#' @export
#' @importFrom ggplot2 labs waiver
#' @importFrom glue glue
labs_ext <- function(title = ggplot2::waiver(),
                     subtitle = ggplot2::waiver(),
                     caption = ggplot2::waiver(),
                     tag = ggplot2::waiver(),
                     alt = ggplot2::waiver(),
                     location = NULL,
                     name_col = NULL, # Check param name
                     source = NULL,
                     .na = "NA",
                     .null = NULL,
                     ...) {
  labs_glued <-
    ggplot2::labs(
      title = glue::glue(title, .null = .null),
      subtitle = glue::glue(subtitle, .null = .null),
      caption = glue::glue(caption, .null = .null),
      tag = glue::glue(tag, .null = .null),
      alt = glue::glue(alt, .null = .null),
      ...
    )

  labs_glued
}
