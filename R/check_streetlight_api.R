#' @title Test connectivity and authentication to the API server.
#'
#' @param key character, StreetLight API key. Default is `NULL`.
#'
#' @return a PASS or FAIL message
#'
#' @note Take care of the rate limit on the API. For quick checks, a 1 second break (`Sys.sleep(1)`) is usually sufficient.
#'
#' @importFrom cli cli_alert_success cli_alert_danger
#' @importFrom httr2 req_headers req_body_raw req_perform resp_status_desc
#' @export
#'
check_streetlight_api <- function(key = NULL) {
  # check for API key access
  key <- check_api_key_access(key)


  # send hello world message to debug endpoint
  resp <- streetlight_insight(
    key = key,
    endpoint = "debug/echo"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_body_raw('{"message": "hello world"}') %>%
    httr2::req_perform()


  # provide specific guidance based on response
  if (httr2::resp_status_desc(resp) == "OK") {
    cli::cli_alert_success("API connection live")
    return("PASS")
  } else {
    cli::cli_alert_danger("Something is wrong. Check your API key?")
    httr2::resp_body_raw()
    return("FAIL")
  }
}
