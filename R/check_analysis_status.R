#' Check the status of a analysis in the StreetLight API.
#'
#' @param analysis_name character, analysis name
#' @param analysis_name_ Deprecated, use `analysis_name` parameter.
#' @param analysis_uuid character, unique analysis identifier
#' @inheritParams check_streetlight_api
#' 
#' @note
#'  If `analysis_uuid` is provided, it will be used instead of `analysis_name`
#'  to query status. 
#'
#' @return If successful, a list with two items
#'     - `analyses`
#'     - `status`
#'
#'     Otherwise, an httr2 response.
#'
#' @export
#'
#' @importFrom httr2 req_body_json req_perform req_headers req_error
#' @importFrom cli cli_warn
#' @importFrom purrr map2
#'
check_analysis_status <- function(analysis_name = NULL,
                                  analysis_uuid = NULL,
                                  key = NULL,
                                  analysis_name_ = NULL) {
  # check for API key access
  key <- check_api_key_access(key)
  # validate parameters
  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )

  # print warning if using analysis_name_
  if (!is.null(analysis_name_)) {
    cli::cli_warn(c("`analysis_name_` deprecated. Use 'analysis_name' instead."))
    analysis_name <- analysis_name_
  }

  if (!is.null(analysis_uuid)) {
    req_body <- list(
      analyses = list(list(uuid = analysis_uuid))
    )
  } else {
    req_body <- list(
      analyses = list(list(name = analysis_name))
    )
  }

  # fetch analysis status from endpoint
  resp <- streetlight_insight(
    key = key,
    endpoint = "analyses/status"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_body_json(req_body) %>%
    httr2::req_perform()


  if (httr2::resp_status(resp) != 200) {
    return(
      cli::cli_warn(c(
        "Status failed with message: ",
        httr2::resp_body_json(resp)
      ))
    )
  } else {
    # otherwise, return success
    cli::cli_alert_success(c("Status check succceeded"))
    return(httr2::resp_body_json(resp, check_type = FALSE, simplifyVector = TRUE))
  }
}
