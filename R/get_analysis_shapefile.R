#' @title Get Shapefiles by analysis name
#'
#' @param shapefile character, which Shapefile to fetch. Availability is
#'     dependent on the analysis type and configuration.
#'     See [create_streetlight_analysis()] for more details.
#'
#' @inheritParams check_streetlight_api
#' @inheritParams check_analysis_status
#'
#' @return a [sf::sf] object with columns `id`, `name`, `direction`,
#'   `is_pass`, `is_bidi`, `geometry`, `file_name`, and `shapefile`.
#'
#' @export
#'
#' @importFrom utils  URLencode
#' @importFrom janitor clean_names
#' @importFrom cli cli_alert_success cli_alert_danger
#' @importFrom sf read_sf
#' @importFrom purrr flatten map2
#' @importFrom httr2 req_headers req_error req_perform resp_status_desc resp_body_json
#'
get_analysis_shapefile <- function(analysis_name = NULL,
                                   key = NULL,
                                   shapefile,
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


  if (is.null(analysis_status$analyses$shapefiles[[1]][1]) | analysis_status$analyses$shapefiles[[1]][1] == "NULL") {
    cli::cli_abort("No shapefiles are available for this analysis")
  }

  # check if shapefile matches any available shapefiles
  if (!is.null(shapefile) &
    !shapefile %in% analysis_status$analyses$shapefiles[[1]]) {
    cli::cli_abort(
      c(
        "`shapefile` '{shapefile}' is unavailable",
        "`shapefile` must match one of the available shapefiles for this analysis",
        paste(analysis_status$analyses$shapefiles[[1]], collapse = ", ")
      )
    )
  }


  tmpdir <- tempdir()

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
        shapefile
      )
    ) %>%
      httr2::req_headers(
        "x-stl-key" = key
      ) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform(path = paste0(tmpdir, "/", shapefile, ".zip"))

    # error if no analysis found
    if (httr2::resp_status(resp) == 404) {
      cli::cli_abort("No analysis downloads were found.")
    }

    unzip(paste0(tmpdir, "/", shapefile, ".zip"),
      exdir = paste0(tmpdir, "/", shapefile)
    )

    these_files <- list.files(paste0(tmpdir, "/", shapefile))

    shp_file_name <- stringr::str_sub(these_files[these_files %>% stringr::str_detect(".shp")], start = 1, end = -5)

    shp <- sf::read_sf(paste0(tmpdir, "/", shapefile, "/", shp_file_name, ".shp")) %>%
      dplyr::mutate(
        file_name = shp_file_name,
        shapefile = shapefile
      )

    unlink(tmpdir)

    return(shp)
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
