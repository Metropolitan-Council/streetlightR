#' @title Create a tag
#'
#' @description
#' Tags are useful for organizing analyses by project, organization, user, or anything else!
#'
#' @param tag_name character, the tag you want to create
#' @inheritParams check_streetlight_api
#' @inheritParams create_streetlight_analysis
#'
#' @return If successful, a success message.
#' @family tags
#'
#' @export
#' @importFrom httr2 req_headers req_perform resp_status_desc req_error
#' @importFrom purrr map2
create_tag <- function(key = NULL,
                       login_email,
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
    endpoint = "analyses/tags"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_body_json(
      list(
        "insight_login_email" = login_email,
        "tag_name" = tag_name
      ),
      auto_unbox = TRUE
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  if (!httr2::resp_status_desc(resp) %in% c(
    "created",
    "Created"
  )) {
    return(cli::cli_warn(c(
      "Create tag failed with message:",
      httr2::resp_body_json(resp)
    )))
  }

  # return response json body
  return(httr2::resp_body_json(resp))
}
