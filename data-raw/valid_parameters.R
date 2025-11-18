# valid input values for body params

valid_parameters <- list(
  analysis_type = c(
    "AADT",
    "OD_Analysis",
    "OD_MF_Analysis",
    "OD_Preset_Geography",
    "Segment_Analysis",
    "Top_Routes_OD",
    "Top_Routes_ZA",
    "Zone_Activity_Analysis",
    "Network_Performance",
    "Network_OD"
  ),
  travel_mode_type = c(
    # "All_Vehicles",
    "All_Vehicles_LBS_Plus",
    "All_Vehicles_CVD_Plus",
    "All_Vehicles_By_Weight",
    "All_Vehicles_AGPS",
    "Bus",
    "Rail",
    "Bicycle",
    "Truck",
    "Pedestrian"
  ),
  travel_mode_data = c(
    "LIGHT_DUTY",
    "MEDIUM_DUTY_HEAVY_DUTY"
  ),
  output_type = c(
    "index",
    "volume",
    "trip_counts",
    "zone_counts"
  ),
  unit_of_measurement = c(
    "miles",
    "km"
  ),
  unit = c(
    "mi",
    "km"),
  segment_types = c(
    "Motorway",
    "Trunk",
    "Primary",
    "Secondary",
    "Tertiary",
    "Residential"
  ),
  geography_type = c(
    "zip",
    "taz",
    "blkgrp",
    "da"
  ),
  zone_intersection_type = c(
    "all_trips_for_zone",
    "trips_by_pass_through_setting"
  ),
  metric_type = c(
    "segment",
    "spot"
  ),
  endpoint = c(
    "analyses",
    "analyses/download/name",
    "analyses/status",
    "analyses/tags",
    "debug/echo",
    "tags",
    "tags/remove_analyses_tag",
    "tags/tag_analyses",
    "zone_sets",
    "zone_sets/search",
    "osm_ids/search"
  ),
  country = c(
    "US",
    "CA"
  )
)

usethis::use_data(valid_parameters, overwrite = TRUE)
