#' Add labels to a ggplot2 location map
#'
#' A helper function that converts strings to glue strings for the title,
#' subtitle, and caption. In progress.
#'
#' @inheritParams ggplot2::labs
#' @param location sf or bbox object or character string
#' @param locationname_col Column name holding name or identifier for distinct
#'   places within the simple feature collection provided to location. Not
#'   supported for bbox objects.
#' @param source Data source(s). Not yet used or supported by function.
#' @export
#' @importFrom ggplot2 labs waiver
labs_ext <- function(title = ggplot2::waiver(),
                     subtitle = ggplot2::waiver(),
                     caption = ggplot2::waiver(),
                     tag = ggplot2::waiver(),
                     alt = ggplot2::waiver(),
                     location = NULL,
                     locationname_col = NULL, # Check param name
                     source = NULL,
                     ...) {
  labs_glued <-
    ggplot2::labs(
      title = glue::glue(title),
      subtitle = glue::glue(subtitle),
      caption = glue::glue(caption),
      tag = glue::glue(tag),
      alt = glue::glue(alt),
      ...
    )

  labs_glued
}
