
testthat::test_that("Expected warning returned", {
  testthat::expect_warning(create_streetlight_analysis(
    login_email = httr2::secret_decrypt(
      "Q8yxP3atCs0CbbM_C3VBArmMQpNlcAPAlbRfNR_nBxVT0mcOyi7dLKxB",
      key = "STREETLIGHTR_KEY"
    ),
    key = httr2::secret_decrypt(
      "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
      key = "STREETLIGHTR_KEY"
    ),
    analysis_type = "Zone_Activity_Analysis",
    analysis_name = "v220311-VMT 2019 month 1 Volume Trip Trav Attr",
    travel_mode_type = "All_Vehicles",
    output_type = "volume",
    origin_zone_set = "MPO_Counties",
    trip_attributes = TRUE,
    traveler_attributes = TRUE,
    date_ranges = list(
      start_date = "01/01/2019",
      end_date = "01/31/2019"
    ),
    day_types = "All Days|17,Average Weekday|15,Average Weekend Day|67",
    day_parts = "All Day|0023, Morning|0709, Midday|1113, Evening|1618",
    tags = list("streetlightR")
  ))
})
