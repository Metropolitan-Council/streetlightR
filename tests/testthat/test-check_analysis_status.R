testthat::test_that("Analysis status correct", {
  status <- check_analysis_status(
    analysis_name_ = "testing4",
    key = httr2::secret_decrypt("Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
      key = "STREETLIGHTR_KEY"
    )
  ) %>%
    testthat::expect_warning()

  avail <- status$status
  name <- status$name
  metrics1 <- status$metrics[[1]][1]
  metrics2 <- status$metrics[[1]][2]


  testthat::expect_identical(avail, expected = "Available")
  testthat::expect_identical(name, expected = "testing4")
  testthat::expect_identical(metrics1, expected = "od_all")
  testthat::expect_identical(metrics2, expected = "zone_od_all")
})
