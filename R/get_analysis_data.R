#' @title Get Metric result content for an Analysis by Analysis Name
#'
#' @param metric character, which metric to fetch. Metric availability is
#'     dependent on the analysis type and configuration.
#'     See [create_streetlight_analysis()] for more details.
#' @inheritParams check_streetlight_api
#' @inheritParams check_analysis_status
#'
#' @return a tibble with the requested metric
#' @export
#'
#' @importFrom utils  URLencode
#' @importFrom janitor clean_names
#' @importFrom cli cli_alert_success cli_alert_danger
#' @importFrom readr read_delim
#' @importFrom purrr flatten map2
#' @importFrom httr2 req_headers req_error req_perform resp_status_desc resp_body_json
#'
get_analysis_data <- function(analysis_name = NULL,
                              key = NULL,
                              metric,
                              analysis_name_ = NULL) {
  # check for API key access
  key <- check_api_key_access(key)
  # validate parameters
  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )
  
  # check for deprecated args
  if (!is.null(analysis_name_)) {
    cli::cli_warn(c("`analysis_name_` deprecated. Use 'analysis_name' instead."))
    analysis_name <- analysis_name_
  }

  # check analysis status
  analysis_status <- check_analysis_status(
    analysis_name = analysis_name,
    key = key
  ) %>%
    httr2::resp_body_json(
      check_type = FALSE,
      simplifyVector = TRUE
    )

  # check if metric matches any available metrics
  if (!is.null(metric) &
    !metric %in% analysis_status$analyses$metrics[[1]]) {
    cli::cli_abort(
      c(
        "`metric` '{metric}' is unavailable",
        "`metric` must match one of the available metrics for this analysis",
        paste(analysis_status$metrics[[1]], collapse = ", ")
      )
    )
  }

  # if data available, fetch from endpoint
  if (analysis_status$analyses$status %in% c(
    "Available",
    "Data_Available",
    "Data Available"
  )) {
    resp <- streetlight_insight(
      key = key,
      endpoint = paste0(
        "analyses/download/name/",
        utils::URLencode(analysis_name),
        "/",
        metric
      )
    ) %>%
      httr2::req_headers(
        "x-stl-key" = key
      ) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()

    # error if no analysis found
    if (httr2::resp_status(resp) == 404) {
      cli::cli_abort("No analysis downloads were found.")
    }

    # read in response body as string, convert to tibble, and clean col names
    results_dt <- resp %>%
      httr2::resp_body_string(encoding = "UTF-8") %>%
      readr::read_delim(
        delim = ",",
        show_col_types = FALSE
      ) %>%
      janitor::clean_names()

    return(results_dt)
  } else if (analysis_status$analyses$status %in% c("Cancelled")) {
    # error if analysis was cancelled
    cli::cli_abort("Analysis {analysis_name} was cancelled.")
  } else if (analysis_status$analyses$status %in% c(
    "In_Coverage_Review",
    "In Coverage Review"
  )) {
    # error if in coverage review
    cli::cli_abort("Analysis {analysis_name} in coverage review")
  } else if (analysis_status$analyses$status == "Processing") {
    # error if still processing
    cli::cli_abort("Analysis {analysis_name} is processing")
  } else if (analysis_status$analyses$status == "Deleted") {
    # error if deleted
    cli::cli_abort("Analysis {analysis_name} was deleted.")
  }
}
