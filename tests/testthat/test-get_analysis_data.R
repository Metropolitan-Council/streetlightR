testthat::test_that("VMT download is returned", {
  get_analysis_data(
    analysis_name_ = "v221017-VMT 2022 month 4 Volume Trip Trav Attr",
    metric = "zad"
  ) %>%
    testthat::expect_warning() %>%
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
