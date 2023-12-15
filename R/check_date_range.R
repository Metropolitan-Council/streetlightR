#' @title Check available date ranges for a given travel mode type
#'
#' @description Useful for looking up most recent available data to avoid
#'     errors in analysis creation.
#'
#' @param country character, one of `"US"` or `"CA"`. Default is `"US"`.
#' @inheritParams check_streetlight_api
#' @inheritParams create_streetlight_analysis
#'
#' @return [tibble::tibble()] with columns `travel_mode_type`, `start_date`,
#'    and `end_date`.
#' @export
#'
#' @examples
#' \dontrun{
#' library(streetlightR)
#' check_date_range()
#' }
#'
#' @importFrom httr2 req_url_query req_perform resp_body_json
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select
#'
check_date_range <- function(key = NULL,
                             travel_mode_type = "All_Vehicles",
                             country = "US") {
  # check for API access
  key <- check_api_key_access(key)
  # validate parameters
  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )
  
  # fetch date ranges from endpoint
  resp <- streetlight_insight(
    key = key,
    endpoint =
      paste0(
        "date_ranges/", country, "/",
        travel_mode_type
      )
  ) %>%
    httr2::req_url_query() %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(simplifyVector = TRUE)

  # convert response into tibble
  date_range_table <- resp$date_ranges$supports %>%
    tibble::as_tibble() %>%
    dplyr::mutate(travel_mode_type = travel_mode_type) %>%
    dplyr::select(travel_mode_type, start_date, end_date)

  return(date_range_table)
}
