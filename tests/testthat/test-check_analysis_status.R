testthat::test_that("Analysis status correct", {
  status <- check_analysis_status(
    analysis_name_ = "testing4"
  ) %>%
    httr2::resp_body_json(simplifyVector = TRUE) %>% 
    testthat::expect_warning()

  avail <- status$analyses$status
  name <- status$analyses$name
  metrics1 <- status$analyses$metrics[[1]][1]
  metrics2 <- status$analyses$metrics[[1]][2]


  testthat::expect_identical(avail, expected = "Available")
  testthat::expect_identical(name, expected = "testing4")
  testthat::expect_identical(metrics1, expected = "od_all")
  testthat::expect_identical(metrics2, expected = "zone_od_all")
})
