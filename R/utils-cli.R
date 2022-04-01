
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
