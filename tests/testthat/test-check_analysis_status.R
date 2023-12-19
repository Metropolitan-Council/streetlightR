testthat::test_that("Pt. Douglas Analysis status correct", {
  try_again(3, {
    Sys.sleep(3)
    status <- check_analysis_status(
      analysis_name_ = "Pt Douglas Regional Trail Ped Summer 19-21"
    ) %>%
      httr2::resp_body_json(simplifyVector = TRUE) %>%
      testthat::expect_warning()
  })

  avail <- status$analyses$status
  name <- status$analyses$name
  metrics1 <- status$analyses$metrics[[1]][1]
  metrics2 <- status$analyses$metrics[[1]][2]


  testthat::expect_identical(avail, expected = "Available")
  testthat::expect_identical(name, expected = "Pt Douglas Regional Trail Ped Summer 19-21")
  testthat::expect_identical(metrics1, expected = "za_ped")
  testthat::expect_identical(metrics2, expected = "home_grid_ped")
})



testthat::test_that("TCS Analysis status correct", {
  try_again(times = 4, {
    Sys.sleep(3)
    status <- check_analysis_status(
      analysis_name_ = "TCS Corridors 8 and 9 Calibration"
    ) %>%
      httr2::resp_body_json(simplifyVector = TRUE) %>%
      testthat::expect_warning()
  })

  avail <- status$analyses$status
  name <- status$analyses$name
  metrics1 <- status$analyses$metrics[[1]][1]
  metrics2 <- status$analyses$metrics[[1]][2]


  testthat::expect_identical(avail, expected = "Available")
  testthat::expect_identical(name, expected = "TCS Corridors 8 and 9 Calibration")
  testthat::expect_identical(metrics1, expected = "za_comm")
  testthat::expect_identical(metrics2, expected = "sample_size")
})


testthat::test_that("TCS Analysis status correct", {
  try_again(times = 4, {
    Sys.sleep(3)
    status <- check_analysis_status(
      analysis_name_ = "2020 494 Hwy 100 to Cedar Ave/Hwy 77 OD"
    ) %>%
      httr2::resp_body_json(simplifyVector = TRUE) %>%
      testthat::expect_warning()
  })
  avail <- status$analyses$status
  name <- status$analyses$name
  metrics1 <- status$analyses$metrics[[1]][1]
  metrics2 <- status$analyses$metrics[[1]][2]


  testthat::expect_identical(avail, expected = "Available")
  testthat::expect_identical(name, expected = "2020 494 Hwy 100 to Cedar Ave/Hwy 77 OD")
  testthat::expect_identical(metrics1, expected = "od_all")
  testthat::expect_identical(metrics2, expected = "zone_od_all")
})
