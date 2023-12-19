testthat::test_that("VMT download is returned", {
  get_analysis_data(
    analysis_name_ = "v221017-VMT 2022 month 4 Volume Trip Trav Attr",
    metric = "zad"
  ) %>%
    testthat::expect_warning() %>%
    testthat::expect_error()
})

Sys.sleep(2)

testthat::test_that("Correct VMT download is returned", {
  an_dat <- get_analysis_data(
    analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
    metric = "za_all"
  )
  
  testthat::expect_equal(nrow(an_dat), 4776)
  testthat::expect_equal(ncol(an_dat), 15)
  testthat::expect_equal(names(an_dat), c(
    "data_periods", "mode_of_travel",
    "intersection_type", "zone_id",
    "zone_name", "zone_is_pass_through",
    "zone_direction_degrees",
    "zone_is_bi_direction",
    "day_type",
    "day_part",
    "average_daily_zone_traffic_st_l_volume",
    "avg_travel_time_sec",
    "avg_all_travel_time_sec",
    "avg_trip_length_mi",
    "avg_all_trip_length_mi"
  ))
})

Sys.sleep(2)

testthat::test_that("Correct VMT sample size is returned", {
  an_dat <- get_analysis_data(
    analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
    metric = "sample_size"
  )
  
  testthat::expect_equal(nrow(an_dat), 1)
  testthat::expect_equal(ncol(an_dat), 4)
  testthat::expect_equal(
    names(an_dat),
    c(
      "data_periods", "mode_of_travel",
      "approximate_device_count",
      "approximate_trip_count"
    )
  )
})

Sys.sleep(5)

testthat::test_that("Correct za_all is returned", {
  
  try_again(times = 3, {
    Sys.sleep(2)
    an_dat <- get_analysis_data(
      analysis_name = "Pt Douglas Regional Parking Lot Summer 22",
      metric = "za_all"
    )}
  )
  
  testthat::expect_equal(nrow(an_dat), 24)
  testthat::expect_equal(ncol(an_dat), 15)
  testthat::expect_equal(
    names(an_dat),
    c(
      "data_periods", "mode_of_travel", "intersection_type", "zone_id",
      "zone_name", "zone_is_pass_through", "zone_direction_degrees",
      "zone_is_bi_direction", "day_type", "day_part", "average_daily_zone_traffic_st_l_volume",
      "avg_travel_time_sec", "avg_all_travel_time_sec", "avg_trip_length_mi",
      "avg_all_trip_length_mi"
    )
  )
  
})

Sys.sleep(5)

testthat::test_that("Error on cats", {
  get_analysis_data(
    analysis_name = "Pt Douglas Regional Parking Lot Summer 22",
    metric = "cats"
  ) %>%
    testthat::expect_error()
})


Sys.sleep(0.05)

testthat::test_that("Rate limit hit", {
  get_analysis_data(
    analysis_name = "v221017-VMT 2022 month 4 Volume Trip Trav Attr",
    metric = "zone_trip_all"
  ) %>%
    testthat::expect_error()
})
