#' @title Delete an analysis by specifying its name.
#'
#' @inheritParams check_streetlight_api
#' @inheritParams create_streetlight_analysis
#'
#' @return If successful, a success message, otherwise a warning message.
#' @export
#'
#' @importFrom httr2 req_headers req_perform resp_status_desc req_error
#' @importFrom purrr map2
#' @importFrom cli cli_alert_success cli_warn
delete_analysis <- function(key = NULL,
                            login_email,
                            analysis_name) {
  # check for API key access
  key <- check_api_key_access(key)

  # validate parameters
  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )

  analysis_status <- check_analysis_status(
    analysis_name = analysis_name,
    key = key
  ) %>%
    suppressMessages()


  if (analysis_status$analyses$status %in% c("Processing")) {
    cli::cli_abort(c(
      "Analysis cannot be deleted",
      "Try cancelling instead."
    ))
  }

  # send analysis name in endpoint URL
  resp <- streetlight_insight(
    key = key,
    endpoint = paste0(
      "analyses/",
      utils::URLencode(analysis_name)
    )
  ) %>%
    httr2::req_method("DELETE") %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_body_json(
      list(
        "insight_login_email" = login_email
      ),
      auto_unbox = TRUE
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()


  if (!httr2::resp_status_desc(resp) %in% c(
    "OK"
  )) {
    return(
      cli::cli_warn(c(
        "Delete analysis by name failed with message: ",
        httr2::resp_body_json(resp)
      ))
    )
  } else {
    cli::cli_alert_success(
      c(
        "Delete analysis by name succeeded with message: ",
        httr2::resp_body_json(resp)
      )
    )
  }
}


#' @title Cancel an analysis by specifying its name.
#'
#' @inheritParams check_streetlight_api
#' @inheritParams create_streetlight_analysis
#'
#' @return If successful, a success message, otherwise a warning message.
#' @export
#'
#' @importFrom httr2 req_headers req_perform resp_status_desc req_error
#' @importFrom purrr map2
#' @importFrom cli cli_alert_success cli_warn
cancel_analysis <- function(key = NULL,
                            login_email,
                            analysis_name) {
  # check for API key access
  key <- check_api_key_access(key)

  # validate parameters
  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )

  analysis_status <- check_analysis_status(
    analysis_name = analysis_name,
    key = key
  ) %>%
    suppressMessages()

  if (analysis_status$analyses$status == "Available") {
    return(cli::cli_warn("Only pending or review analyses can be cancelled. "))
  }


  # send analysis name in endpoint URL
  resp <- streetlight_insight(
    key = key,
    endpoint = paste0(
      "analyses/cancel/",
      utils::URLencode(analysis_name)
    )
  ) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_body_json(
      list(
        "insight_login_email" = login_email
      ),
      auto_unbox = TRUE
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()


  if (!httr2::resp_status_desc(resp) %in% c(
    "OK"
  )) {
    return(
      cli::cli_warn(c(
        "Delete analysis by name failed with message: ",
        ifelse(httr2::resp_content_type(resp) == "application/json",
          httr2::resp_body_json(resp),
          httr2::resp_body_html(resp)
        )
      ))
    )
  } else {
    cli::cli_alert_success(
      c(
        "Delete analysis by name succeeded with message: ",
        httr2::resp_body_json(resp)
      )
    )
  }
}
