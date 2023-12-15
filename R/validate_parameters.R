#' Validate parameters
#'
#' @param param character, parameter as character
#' @param value any, parameter value
#'
#' @return Throws error if value is invalid
#' @export
#'
#' @examples
#'
#' validate_parameters("analysis_type", "Zone_Activity_Analysis")
#'
#' @importFrom cli cli_abort
#' @keywords internal
validate_parameters <- function(param,
                                value) {
  # check string values -----
  if (param %in% c(
    "insight_login_email",
    "analysis_name",
    "description",
    "destination_zone_set",
    "origin_zone_set",
    "analysis_type",
    "travel_mode_type",
    "output_type",
    "day_parts",
    "day_types",
    "unit_of_measurement",
    "geography_type",
    "segment_types",
    "trip_speed_bins",
    "trip_duration_bins",
    "trip_length_bins",
    "trip_circuity_bins",
    "speed_percentile_bins",
    "zone_intersection_type",
    "metric"
  )) {
    if (class(value) != "character") {
      cli::cli_abort(paste0(
        "Parameter '",
        param, "' must be a string."
      ))
    }
  }

  # check integer  -----
  if (param %in% c(
    "aadt_year",
    "aadt_calibration_year"
  )) {
    if (class(value) != "numeric") {
      cli::cli_abort(paste0(
        "Parameter '",
        param, "' must be a numeric."
      ))
    }
  }

  # check logical----
  if (param %in% c(
    "traveler_attributes",
    "trip_attributes",
    "enable_speed_percentile",
    "enable_home_work_locations",
    "hwl_enable_visitor",
    "hwl_enable_resident",
    "hwl_enable_worker",
    "is_massive_queue",
    "enable_15min",
    "enable_upsampling",
    "enable_visualization",
    "enable_completion_email"
  )) {
    if (class(value) != "logical") {
      cli::cli_abort(paste0(
        "Parameter '",
        param, "' must be a TRUE or FALSE"
      ))
    }
  }


  # Specific parameter values -----
  ## check analysis_type -----
  if (param == "analysis_type") {
    if (!value %in% valid_parameters$analysis_type) {
      cli::cli_abort(paste0(
        "Parameter '",
        param, "' must be one of: ",
        paste0(
          collapse = ", ",
          valid_parameters$analysis_type
        )
      ))
    }
  }


  ## check output_type-----
  if (param == "output_type") {
    if (!value %in% valid_parameters$output_type) {
      cli::cli_abort(paste0(
        "Parameter '",
        param, "' must be one of: ",
        paste0(
          collapse = ", ",
          valid_parameters$output_type
        )
      ))
    }
  }

  ## check travel_mode_type -----
  if (param == "travel_mode_type") {
    if (!value %in% valid_parameters$travel_mode_type) {
      cli::cli_abort(paste0(
        "Parameter '",
        param, "' must be one of: ",
        paste0(
          collapse = ", ",
          valid_parameters$travel_mode_type
        )
      ))
    }
  }

  if (param == "unit_of_measurement") {
    if (!value %in% valid_parameters$unit_of_measurement) {
      cli::cli_abort(paste0(
        "Parameter '",
        param, "' must be one of: ",
        paste0(
          collapse = ", ",
          valid_parameters$unit_of_measurement
        )
      ))
    }
  }

  if (param == "geography_type") {
    if (!value %in% valid_parameters$geography_type) {
      cli::cli_abort(paste0(
        "Parameter '",
        param, "' must be one of: ",
        paste0(
          collapse = ", ",
          valid_parameters$geography_type
        )
      ))
    }
  }

  if (param == "zone_intersection_type") {
    if (!value %in% valid_parameters$zone_intersection_type) {
      cli::cli_abort(paste0(
        "Parameter '",
        param, "' must be one of: ",
        paste0(
          collapse = ", ",
          valid_parameters$zone_intersection_type
        )
      ))
    }
  }
}
