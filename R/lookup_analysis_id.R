#' @title Look-up analysis name or UUID
#'
#' @description
#' Provide either `analysis_name` or `uuid` and the function will return the
#' corresponding `analysis_name` or `uuid`
#'
#' @inheritParams create_streetlight_analysis
#' @param uuid character, UUID for a given analysis.
#'
#' @return character, the corresponding UUID or analysis name
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(streetlightR)
#'
#' lookup_analysis_id(analysis_name = "Pt Douglas Regional Parking Lot Summer 22")
#' #> "89ce3700-872e-40d5-981b-019fe0663884"
#'
#' lookup_analysis_id(uuid = "89ce3700-872e-40d5-981b-019fe0663884")
#' #> "Pt Douglas Regional Parking Lot Summer 22"
#' }
lookup_analysis_id <- function(analysis_name = NULL,
                               uuid = NULL,
                               key = NULL) {
  # check for API key access
  key <- check_api_key_access(key)
  # validate parameters
  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )

  if (is.null(uuid)) {
    body_list <- list(
      analyses = list(list(name = analysis_name))
    )

    return_val <- "uuid"
  } else if (is.null(analysis_name)) {
    body_list <- list(
      analyses = list(list(uuid = uuid))
    )

    return_val <- "name"
  }

  resp <- streetlight_insight(
    key = key,
    endpoint = "analyses/status"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_body_json(body_list) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(
      simplifyVector = TRUE,
      check_type = FALSE
    )


  if (return_val == "uuid") {
    return(resp$analyses$uuid)
  } else if (return_val == "name") {
    return(resp$analyses$name)
  }
}
