# valid input values for body params

valid_parameters <- list(
  analysis_type = list(
    "Zone_Activity_Analysis",
    "Segment_Analysis",
    "OD_Analysis",
    "OD_MF_Analysis",
    "OD_Preset_Geography",
    "AADT",
    "Top_Routes_OD",
    "Top_Routes_ZA"
  ),
  travel_mode_type = list(
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
  output_type = list(
    "index",
    "volume",
    "trip_counts",
    "aadt",
    "zone_counts"
  ),
  unit_of_measurement = list(
    "miles",
    "km"
  ),
  segment_types = list(
    "Motorway",
    "Trunk",
    "Primary",
    "Secondary",
    "Tertiary",
    "Residential"
  ),
  geography_type = list(
    "zip",
    "taz",
    "blkgrp",
    "da"
  ),
  zone_intersection_type = list(
    "all_trips_for_zone",
    "trips_by_pass_through_setting"
  )
)

usethis::use_data(valid_parameters, overwrite = TRUE)
