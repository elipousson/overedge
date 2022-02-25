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
#' @export
#' @importFrom ggplot2 labs
labs_ext <- function(
    title = waiver(),
    subtitle = waiver(),
    caption = waiver(),
    tag = waiver(),
    alt = waiver(),
    alt_insight = waiver(),
    location = NULL,
    locationname_col = NULL, # Check param name
    source = NULL) {

  labs_glued <-
    ggplot2::labs(
      title = glue::glue(title),
      subtitle = glue::glue(subtitle),
      caption = glue::glue(caption),
      tag = glue::glue(tag),
      alt = alt,
      alt_insight = alt_insight
    )

  labs_glued
}
