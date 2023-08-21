testthat::test_that("Expected warning returned", {
  testthat::expect_warning(
    create_streetlight_analysis(
      login_email = test_login,
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
    )
  )
})

Sys.sleep(1)

testthat::test_that("Expected second warning returned", {
  testthat::expect_warning(
    create_streetlight_analysis(
      login_email = test_login,
      analysis_type = "Zone_Activity_Analysis",
      analysis_name = "v23-VMT 2021 month 1 Volume Trip Trav Attr",
      travel_mode_type = "All_Vehicles",
      output_type = "volume",
      origin_zone_set = "MPO_Counties",
      trip_attributes = FALSE,
      traveler_attributes = TRUE,
      date_ranges = list(
        start_date = "01/01/2021",
        end_date = "01/31/2021"
      ),
      day_types = "All Days|17,Average Weekday|15,Average Weekend Day|67",
      day_parts = "All Day|0023, Morning|0709, Midday|1113, Evening|1618",
      tags = list("streetlightR")
    )
  )
})
