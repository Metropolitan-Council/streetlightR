test_that("parameters validate correctly", {
  validate_parameters("analysis_type", "OD_An") %>%
    testthat::expect_error()

  validate_parameters("insight_login_email", "OD_An") %>%
    testthat::expect_no_error()

  validate_parameters("output_type", "vol") %>%
    testthat::expect_error()

  validate_parameters(
    "date_ranges",
    list(
      start_date = "01/01/2021",
      end_date = "12/31/2021"
    )
  ) %>%
    testthat::expect_no_error()
})
