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
})


Sys.sleep(0.05)

testthat::test_that("Rate limit hit", {
  get_analysis_data(
    analysis_name = "v221017-VMT 2022 month 4 Volume Trip Trav Attr",
    metric = "zone_trip_all"
  ) %>%
    testthat::expect_error()
})
