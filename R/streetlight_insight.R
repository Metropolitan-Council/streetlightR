#' @title StreetLight Insights API wrapper
#'
#' @param endpoint character, API endpoint. One of
#'     `c("analyses/status",  "zone_sets", "debug/echo",
#'        "analyses/download/name")`
#' @inheritParams check_streetlight_api
#'
#' @return [httr2::request()] with path and key specifications
#' @export
#'
#' @keywords internal
#'
#' @importFrom httr2 request req_url_path_append req_headers
#'
streetlight_insight <- function(key, endpoint) {
  return(
    # create httr2::request with the StL API url
    httr2::request("https://insight.streetlightdata.com:/api/v2/") %>%
      # append endpoints
      httr2::req_url_path_append(endpoint) %>%
      # add key as header
      httr2::req_headers(
        "x-stl-key" = key
      )
  )
}
