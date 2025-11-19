testthat::test_that("Expect deletion or cancelation", {
  # longer sleep time to give the platform time to start processing
  Sys.sleep(30)

  cancel_analysis(
    login_email = test_login,
    analysis_name = test_analysis_name
  ) %>%
    expect_no_warning()


  Sys.sleep(2)

  delete_analysis(
    login_email = test_login,
    analysis_name = test_analysis_name
  ) %>%
    expect_no_warning()
})
