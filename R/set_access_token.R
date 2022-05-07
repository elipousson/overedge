#' Set or get an access token or API key to/from environment variables.
#'
#' Based on the [mapboxapi::mb_access_token] function from the {mapboxapi}
#' package by Kyle Walker.
#'
#' @param token An access token or API key; required for [set_access_token()].
#'   If token is not provided; type is required for [get_access_token()].
#' @param overwrite If `TRUE`, overwrite existing token; Default: `FALSE`
#' @param install If `TRUE`, install token for use in future sessions; Default:
#'   `FALSE`
#' @param type Name of token; defaults to `NULL`.
#' @rdname set_access_token
#' @export
#'
#' @importFrom utils read.table write.table
set_access_token <- function(token, overwrite = FALSE, install = FALSE, type = NULL) {
  if (!is.character(token)) {
    cli::cli_abort("Your supplied token key appears to be invalid.")
  }

  if (is.null(type)) {
    cli::cli_abort("A type is required.")
  }

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (file.exists(renv)) {
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if (!file.exists(renv)) {
      file.create(renv)
    } else {
      if (isTRUE(overwrite)) {
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv <- read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep(type, oldenv), ]
        write.table(newenv, renv,
          quote = FALSE, sep = "\n",
          col.names = FALSE, row.names = FALSE
        )
      } else {
        tv <- readLines(renv)
        if (any(grepl(type, tv))) {
          stop(sprintf("A %s already exists. You can overwrite it with the argument overwrite=TRUE", type), call. = FALSE)
        }
      }
    }

    token_concat <- paste0(sprintf("%s='", type), token, "'")
    # Append access token to .Renviron file
    write(token_concat, renv, sep = "\n", append = TRUE)
    message(sprintf('Your token has been stored in your .Renviron and can be accessed by Sys.getenv("%s"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`', type))
    return(token)
  } else {
    message("To install your token for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(type = token)
  }
}

#' @name get_access_token
#' @rdname set_access_token
#' @export
get_access_token <- function(token = NULL, type = NULL) {
  if (is.null(token)) {
    # Use public token first, then secret token
    if (Sys.getenv(type) != "") {
      token <- Sys.getenv(type)
    }
  }

  return(token)
}
