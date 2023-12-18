#' @title List all available tags
#' 
#' @description
#' Tags are useful for organizing analyses by project, organization, user, or anything else!
#' 
#' @param tag_name character, the tag you want to create
#' @inheritParams check_streetlight_api
#' @inheritParams create_streetlight_analysis
#' 
#' @return [tibble::tibble()] with columns `analysis_num`, `created_by`, `created_date`,
#'    and `tag_name`.
#'    
#' @export
#' @family tags
#' @importFrom httr2 req_url_query req_perform resp_body_json
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select
#' @importFrom purrr map2
list_available_tags <- function(key = NULL){
  
  # check for API key access
  key <- check_api_key_access(key)
  
  # validate parameters
  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )
  
  resp <- streetlight_insight(
    key = key,
    endpoint = "tags"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_url_query() %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(simplifyVector = TRUE)
  
  
 tags <- resp$tags %>%
    tibble::as_tibble()
  
  # tibble
  return(tags)
  
}