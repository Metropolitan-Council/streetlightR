#' @title Remove a specific tag from an analysis
#'
#' @inheritParams check_streetlight_api
#' @inheritParams create_streetlight_analysis
#' @inheritParams create_tag
#'
#' @return If successful, a success message
#' @export
#'
#' @importFrom httr2 req_headers req_perform resp_status_desc req_error
#' @importFrom purrr map2
#' @importFrom cli cli_alert_success cli_warn
remove_analysis_tag <- function(key = NULL,
                                login_email,
                                analysis_name,
                                tag_name) {
  # check for API key access
  key <- check_api_key_access(key)

  # validate parameters
  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )

  # send tag list to endpoint
  resp <- streetlight_insight(
    key = key,
    endpoint = "tags/remove_analyses_tag"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_body_json(
      list(
        "insight_login_email" = login_email,
        "tags" = list(tag_name),
        "analyses" = list(list("name" = analysis_name))
      ),
      auto_unbox = TRUE
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()


  if (!httr2::resp_status_desc(resp) %in% c(
    "OK"
  )) {
    return(cli::cli_warn(c(
      "Remove tag failed with message: ",
      httr2::resp_body_json(resp)
    )))
  } else {
    cli::cli_alert_success(c(
      "Remove tag succeeded with message: ",
      httr2::resp_body_json(resp)
    ))
  }
}
