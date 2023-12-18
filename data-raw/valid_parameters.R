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
    "Zone_Activity_Analysis"
  ),
  travel_mode_type = c(
    "All_Vehicles",
    "All_Vehicles_LBS_Plus",
    "All_Vehicles_CVD_Plus",
    "All_Vehicles_By_Weight",
    "Bus",
    "Rail",
    "Bicycle",
    "Truck",
    "Pedestrian"
  ),
  output_type = c(
    "index",
    "volume",
    "trip_counts",
    "aadt",
    "zone_counts"
  ),
  unit_of_measurement = c(
    "miles",
    "km"
  ),
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
    "zone_sets/search"
  ),
  country = c(
    "US",
    "CA"
  )
)

usethis::use_data(valid_parameters, overwrite = TRUE)
