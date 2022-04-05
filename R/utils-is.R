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

#' Is the df object empty (no rows)?
#'
#' @noRd
is_df_empty <- function(x, message = "This simple feature object has no rows.", quiet = FALSE) {
  is_empty <- (!is.null(x) && (nrow(x) == 0))

  if (is_empty && !quiet) {
    return(cli::cli_abort(message))
  } else if (!quiet) {
    return(NULL)
  }

  return(is_empty)
}
