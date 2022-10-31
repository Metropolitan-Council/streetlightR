#' @title Check that there are under 7,000 zones in a given zone set
#' @inheritParams upload_zone_set
#' @keywords internal
#' @return If too many zones, error.
check_zone_size <- function(zones) {
  # if zones is an "sf"
  if (class(zones)[[1]] == "sf") {
    # check that there are no more than 7000 rows
    if (nrow(zones) >= 7000) {
      # if 7000 or more, return error
      cli::cli_abort("There are too many zones in this zone set.")
    }
  }
}


#' @title Check API key existence
#'
#' @inheritParams check_streetlight_api
#'
#' @return API key
#' @keywords internal
check_api_key_access <- function(key = NULL) {
  # check for STREETLIGHT_API_KEY in environment
  if (Sys.getenv("STREETLIGHT_API_KEY") != "") {
    # if it exists, fetch and return
    key <- Sys.getenv("STREETLIGHT_API_KEY")
    return(key)
  } else if (is.null(key)) {
    # if not, return error with instructions
    cli::cli_abort("A StreetLight API key is required. Use `streetlight_api_key()` to set it up.")
  }
}
