#' Install a StreetLight API key in your `.Renviron` for repeated use
#'
#' @param key The API key provided to you from StreetLight formated in quotes.
#' @param install logical, if `TRUE`, will install the key in your `.Renviron`
#'     file for use in future sessions. Default is `FALSE`.
#' @param overwrite logical, If this is set to TRUE, it will overwrite an existing STREETLIGHT_API_KEY
#'     that you already have in your `.Renviron` file.
#'
#' @importFrom utils write.table read.table
#' @note Adapted from [tidycensus::census_key]()
#' @export
#'
#' @examples
#' \dontrun{
#' streetlight_key("111111abc", install = TRUE)
#' # First time, reload your environment so you can use the key without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("STREETLIGHT_API_KEY")
#' }
#'
#' \dontrun{
#' # If you need to overwrite an existing key:
#' streetlight_key("111111abc", overwrite = TRUE, install = TRUE)
#' # First time, reload your environment so you can use the key without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("STREETLIGHT_API_KEY")
#' }
#' @importFrom cli cli_alert_info cli_inform cli_abort
#'
streetlight_api_key <- function(key,
                                overwrite = FALSE,
                                install = FALSE) {
  # if installing on system
  if (install) {
    # find home location
    home <- Sys.getenv("HOME")

    # find .Renviron
    renv <- file.path(home, ".Renviron")

    if (file.exists(renv)) {
      # if .Renviron exists, make a copy
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }

    if (!file.exists(renv)) {
      # if .Renviron doesn't exist, make a new .Renviron
      file.create(renv)
    } else {
      if (isTRUE(overwrite)) {
        # if overwrite is true, write new STREETLIGHT_API_KEY
        cli::cli_alert_info("Your original .Renviron is backed up and stored in your R HOME directory ({home}).")

        # get old and new environments
        oldenv <- read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("STREETLIGHT_API_KEY", oldenv), ]

        # write new .Renviron
        write.table(newenv, renv,
          quote = FALSE, sep = "\n",
          col.names = FALSE, row.names = FALSE
        )
      } else {
        # get existing .Renviron lines
        tv <- readLines(renv)

        # check for STREETLIGHT_API_KEY
        if (any(grepl("STREETLIGHT_API_KEY", tv))) {
          # if a key already exists and overwrite = FALSE,  error
          cli::cli_abort(c(
            "x" = "A STREETLIGHT_API_KEY already exists in your .Renviron",
            "i" = "You can overwrite it with the argument `overwrite = TRUE`"
          ))
        }
      }
    }

    keyconcat <- paste0("STREETLIGHT_API_KEY='", key, "'")

    # append new key to environment
    write(keyconcat, renv, sep = "\n", append = TRUE)

    cli::cli_inform(
      c(
        "i" = "Your API key has been stored in your .Renviron.",
        "i" = "Access it with `Sys.getenv(\"STREETLIGHT_API_KEY\")`.",
        "i" = "\nTo use now, restart R or run `readRenviron(\"~/.Renviron\")`"
      )
    )

    return(key)
  } else {
    # set temporary environment
    cli::cli_inform(c(
      "i" = "Your API key has been added to your environment for this session only.",
      "i" = "To install your API key for use in future sessions,
      run this function with `install = TRUE`."
    ))
    Sys.setenv(STREETLIGHT_API_KEY = key)
  }
}
