#' Check the review status of a analysis in the StreetLight API.
#'
#' @inheritParams check_streetlight_api
#' @inheritParams lookup_analysis_id
#' @inheritParams create_streetlight_analysis
#'
#'
#' @return If successful, a list with two items
#'     Otherwise, an httr2 response.
#'
#' @export
#'
#' @importFrom httr2 req_body_json req_perform req_headers req_error
#' @importFrom cli cli_warn cli_alert
#' @importFrom purrr map2
#'
check_analysis_review <- function(analysis_name = NULL,
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
  
  # if no uuid, lookup uuid
  if(is.null(uuid)){
    uuid <- lookup_analysis_id(analysis_name = analysis_name,
                               uuid = uuid)
  }
  
  # fetch analysis status from endpoint
  resp <- streetlight_insight(
    key = key,
    endpoint = paste0("analyses/",
                      uuid, 
                      "/review_detail")
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()
  
  
  # if error message returnd, return WARNING
  if (httr2::resp_status(resp) != 200) { 
    return(
      cli::cli_warn(c(
        "Review check failed with message: ",
        httr2::resp_body_json(resp)
    )))
  } else {
    # otherwise, return succ
    return(cli::cli_alert(c(
      "Review check succceeded with message: ",
      httr2::resp_body_json(resp)
    )))
    
  }
}


