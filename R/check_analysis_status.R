#' Check the status of a analysis in the StreetLight API.
#'
#' @param analysis_name character, analysis name
#' @param analysis_name_ Deprecated, use `analysis_name` parameter.
#' @inheritParams check_streetlight_api
#'
#'
#' @return a the parsed response with the analysis availability, metrics, name, and UUID
#' @export
#'
#' @importFrom httr2 req_body_json req_perform req_headers
#' @importFrom cli cli_warn
#'
check_analysis_status <- function(analysis_name = NULL,
                                  key = NULL,
                                  analysis_name_ = NULL) {
  # check for API key access
  key <- check_api_key_access(key)

  # print warning if using analysis_name_
  if (!is.null(analysis_name_)) {
    cli::cli_warn(c("`analysis_name_` deprecated. Use 'analysis_name' instead."))
    analysis_name <- analysis_name_
  }

  # fetch analysis status from endpoint
  out <- streetlight_insight(
    key = key,
    endpoint = "analyses/status"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_body_json(list(
      analyses = list(list(name = analysis_name))
    )) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(simplifyVector = TRUE)

  return(out$analyses)
}
