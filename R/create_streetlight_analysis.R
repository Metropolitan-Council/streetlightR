#' @title Create a StreetLight analysis, which analyzes traffic or activity with respect to the analysis Zones.
#'
#' @param login_email character, Your StreetLight login email
#' @param analysis_type character, What type of analysis to run. Options are `OD_Analysis`, `OD_MF_Analysis`,
#'  `Zone_Activity_Analysis`,`OD_Preset_Geography`, `Segment_Analysis`, `AADT`, `Top_Routes_OD`,
#'  `Top_Routes_ZA`, or `Traffic_Diagnostics`.
#' @param analysis_name character, The analysis name
#' @param travel_mode_type character, `All_Vehicles`,  `Truck`, `Bicycle`, or `Pedestrian`. Default is `All Vehicles`
#' @param description character, Optional analysis description
#' @param origin_zone_set character, The name of uploaded zone set to use as the origin in an origin-destination analysis or the main zone in a zone activity analysis
#' @param destination_zone_set character, The name of uploaded zone set to use as the destination in an origin-destination analysis.
#' @param middle_zone_set character, The name of uploaded zone set to use as the middle filter in an origin-destination with middle filter analysis.
#' @param geography_type character, Required in O-D to Pre-set Geography analyses. This property is a string of one of `zip`, `taz`, `da`, or `blkgrp`.
#' @param date_ranges list, a list of date ranges. Each date range is an object containing a pair of MM/DD/YYYY dates, with the `start_date` key containing the start of the date range,
#'   and the `end_date` key containing the end of the date range. Both `start_date` and `end_date` are inclusive. Default is all 2019 months.
#'   When `travel_mode_type` is `Bicycle` or `Pedestrian`, each date range must consist of only full months.
#' @param day_types character, a comma-separated list of day types in the analysis. Each day type has a name separated by the vertical bar
#'   from the start day of week to the end day of week (1 for Monday through 7 for Sunday).
#'   Analysis must define All Days as `17` (Monday through Sunday), and they must define values for
#'   Average Weekday and Average Weekend Day. Default includes All Days, Average Weekday, and Average Weekend Day.
#' @param day_parts character,A comma-separated list of day parts in the analysis. Each day part has a name separated
#'  by the vertical bar from the start hour and end hour from 00 (midnight) to 23 (11 PM).
#'  For example, `All Day|0023` ranges from midnight (00:00) to 11:59 PM. Analyses must define All Day as 0023 (midnight to midnight).
#'  Default includes All Day, Recreation Hours (8am-8pm), Park Hours I, II, and III.
#' @param traveler_attributes logical, This property controls whether the analysis results will
#'  include the add-on traveler attribute Metrics. Traveler attribute Metrics include traveler demographics and simple trip purpose.
#'  Default is FALSE.
#' @param trip_attributes logical, This property controls whether the analysis results will include the add-on trip attribute Metrics:
#'  trip time distribution, trip length distribution, trip speed distribution, and trip circuity distribution.
#'  If you specify this, you can also customize the following properties: - `trip_duration_bins` -  `trip_length_bins` -
#'   `trips_speed_bins` - `trip_circuity_bins`. Default is FALSE.
#' @param traveler_attributes logical, whether the analysis results will include the add-on traveler attribute metrics.
#'   Traveler attribute metrics include traveler demographics and simple trip purpose.
#'  (visitor income, education, race, and family status) are included in the Metric results. Default is FALSE.
#' @param is_ui_enabled This allows analysis results to be downloaded and visualized though the UI as well as the API. Should be used sparingly.
#' @param output_type character, One of `volume`, `trip_counts`, `aadt`, `index`, or `zone_counts`. Default is `index`. For Traffic Diagnostics, `index` is allowed.
#' @param aadt_zone_set character, The name of uploaded zone set to use in an analysis with AADT output.
#' @param calibration_zone_set character, name of uploaded zone set with calibration. Required when creating an Analysis with Zone Counts output
#' @param hwl_enable_visitor logical, whether the Analysis results will include visiting trips that neither reside or work in the zone.
#'   It applies only to Zone Activity Analysis with Home and Work Locations metrics enabled.
#' @param hwl_enable_resident logical, whether the Analysis results will include trips that reside in the zone.
#'   It applies only to Zone Activity Analysis with Home and Work Locations metrics enabled.
#' @param hwl_enable_worker logical, whether the Analysis results will include trips that work in the zone.
#'   It applies only to Zone Activity Analysis with Home and Work Locations metrics enabled.
#' @param aadt_calibration_year character, one of `2017`, `2018`, or `2019`. Required integer creating an AADT Analysis.
#' @param enable_visualization logical, whether to enable visualization on the StreetLight platform
#' @param aadt_year character, one of `2017`, `2018`, or `2019`. Required integer creating an AADT Analysis.
#' @param tags list, tag names created within an Organization to associate with the created Analysis. Default is `list("streetlightR")`
#' @param enable_15min logical, whether the Analysis will analyze in 15-minute day parts.
#' @param enable_upsampling logical, whether the Analysis will process with upsampling if it meets the necessary thresholds.
#'   This setting only applies to analysis with `enable_15min` enabled.
#' @param enable_home_work_locations logical, whether the Analysis results will include Home and Work Locations metrics.
#'   If this is `TRUE`, then one of the `hwl_...` parameters must be `TRUE`. Default is `FALSE`.
#' @param zone_intersection_type character, one of `all_trips_for_zone` or `trips_by_pass_through_setting`.
#'   Applies only to Zone Activity Analysis with Home and Work Locations metrics enabled) r
#' @param is_massive_queue logical, whether the Analysis will process alongside other high volume Analyses in order to optimize calculation time.
#' @param segment_types list, must contain at least one of `Motorway`, `Trunk`, `Primary`, `Secondary`, `Tertiary`, `Residential`.
#' @param vehicle_weight character, one of `Medium`, `Heavy`, `Medium,Heavy` or `null`.
#'   Whether metric results for the analysis are broken down
#'   by vehicle weight class (medium duty, heavy duty) for commercial vehicles.
#'   If its value is null, commercial vehicle results are not broken down by vehicle weight class.
#'   Required when creating a Top Routes between Origins and Destinations Analysis or Top Routes for Zones Analysis.
#' @inheritParams check_streetlight_api
#'
#' @return if successful, a list with the analysis name, status, and universal unique ID (uuid).
#' @export
#'
#' @importFrom httr2 req_headers req_perform resp_status_desc req_error
#'
create_streetlight_analysis <- function(login_email,
                                        key = NULL,
                                        analysis_type,
                                        analysis_name,
                                        travel_mode_type = "All_Vehicles",
                                        output_type = "index",
                                        description = "",
                                        origin_zone_set,
                                        destination_zone_set = NA,
                                        middle_zone_set = NA,
                                        aadt_zone_set = NA,
                                        calibration_zone_set = NA,
                                        geography_type = "",
                                        zone_intersection_type = "",
                                        date_ranges = list(
                                          start_date = "01/01/2019",
                                          end_date = "12/31/2019"
                                        ),
                                        day_types = "All Days|17,Average Weekday|15,Average Weekend Day|67",
                                        day_parts = "All Day|0023,Early AM|0005,Peak AM|0609,Mid-Day|1014,Peak PM|1518,Late PM|1923,Recreation Hours|0819",
                                        vehicle_weight = "",
                                        segment_types = list(),
                                        trip_attributes = FALSE,
                                        traveler_attributes = FALSE,
                                        enable_home_work_locations = FALSE,
                                        hwl_enable_visitor = FALSE,
                                        hwl_enable_resident = FALSE,
                                        hwl_enable_worker = FALSE,
                                        aadt_year = "",
                                        aadt_calibration_year = "",
                                        tags = "streetlightR",
                                        is_ui_enabled = FALSE,
                                        enable_visualization = FALSE,
                                        enable_15min = FALSE,
                                        enable_upsampling = TRUE,
                                        is_massive_queue = FALSE) {
  # check for API key access
  key <- check_api_key_access(key)

  # create zone list based on analysis type
  zone_list <- if (analysis_type == "Zone_Activity_Analysis") {
    # if ZAA, only include origin_zone_set
    list("oz_sets" = list(list(name = origin_zone_set)))
  } else if (analysis_type == "OD_MF_Analysis") {
    # if OD with middle filter, include origin, destination, and middle
    list(
      "oz_sets" = list(list(name = origin_zone_set)),
      "dz_sets" = list(list(name = destination_zone_set)),
      "mf_sets" = list(list(name = middle_zone_set))
    )
  } else if (analysis_type == "OD_Analysis") {
    # if OD, including origin and destination
    list(
      "oz_sets" = list(list(name = origin_zone_set)),
      "dz_sets" = list(list(name = destination_zone_set))
    )
  } else if (analysis_type == "OD_Preset_Geography") {
    # if OD pre-set geography, use origin and geography type
    list(
      "oz_sets" = list(list(name = origin_zone_set)),
      "geography_type" = geography_type
    )
  } else if (analysis_type == "AADT") {
    # if aadt, use origin, aadt zone set, and aadt_year
    list(
      "oz_sets" = list(list(name = origin_zone_set)),
      "az_sets" = list(list(name = aadt_zone_set)),
      "aadt_year" = aadt_year
    )
  } else if (analysis_type == "Segment_Analysis") {
    # if segment, include origin zones and segment types
    list(
      "oz_sets" = list(list(name = origin_zone_set)),
      "segment_types" = segment_types
    )
  } else if (analysis_type == "Top_Routes_OD") {
    # if top routes, including origin and destination
    list(
      "oz_sets" = list(list(name = origin_zone_set)),
      "dz_sets" = list(list(name = destination_zone_set))
    )
  } else if (analysis_type == "Top_Routes_ZA") {
    # if top routes with zone activity, include origin and destination
    list(
      "oz_sets" = list(list(name = origin_zone_set)),
      "dz_sets" = list(list(name = destination_zone_set))
    )
  } else if (analysis_type == "Traffic_Diagnostics") {
    # if traffic diagnostics, including origin and destination
    list(
      "oz_sets" = list(list(name = origin_zone_set)),
      "dz_sets" = list(list(name = destination_zone_set))
    )
  }


  # create analysis list from use inputs
  analysis_list <-
    append(
      list(
        "insight_login_email" = login_email,
        "analysis_name" = analysis_name,
        "analysis_type" = analysis_type,
        "travel_mode_type" = travel_mode_type,
        "output_type" = output_type,
        "description" = description,
        "date_ranges" = list(date_ranges),
        "day_types" = day_types,
        "day_parts" = day_parts,
        "trip_attributes" = trip_attributes,
        "traveler_attributes" = traveler_attributes,
        "is_ui_enabled" = is_ui_enabled,
        "enable_home_work_locations" = enable_home_work_locations,
        "hwl_enable_visitor" = hwl_enable_visitor,
        "hwl_enable_resident" = hwl_enable_resident,
        "hwl_enable_worker" = hwl_enable_worker,
        "enable_visualization" = enable_visualization,
        "aadt_calibration_year" = aadt_calibration_year,
        "tags" = tags,
        # "vehicle_weight" = vehicle_weight,
        "segment_types" = segment_types,
        "enable_15min" = enable_15min,
        "is_massive_queue" = is_massive_queue,
        "zone_intersection_type" = zone_intersection_type
      ),
      zone_list
    )

  # send analysis list to endpoint
  resp <- streetlight_insight(
    key = key,
    endpoint = "analyses"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_body_json(analysis_list,
      auto_unbox = TRUE
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # return message based on response
  if (!httr2::resp_status_desc(resp) %in% c(
    "success",
    "created",
    "Success",
    "Created"
  )) {
    return(cli::cli_warn(c(
      "Create analysis failed with message:",
      httr2::resp_body_json(resp)
    )))
  }

  # return response json body
  return(httr2::resp_body_json(resp))
}
