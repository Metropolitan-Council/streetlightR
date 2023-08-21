testthat::test_that("Default travel mode date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range()
    )
  )
})

Sys.sleep(1)

testthat::test_that("All_Vehicles_LBS_Plus date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "All_Vehicles_LBS_Plus"
      )
    )
  )
})

Sys.sleep(1)

testthat::test_that("All_Vehicles_CVD_Plus date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "All_Vehicles_CVD_Plus"
      )
    )
  )
})


Sys.sleep(1)

testthat::test_that("Bike date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Bicycle"
      )
    )
  )
})

Sys.sleep(1)

testthat::test_that("Truck date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Truck"
      )
    )
  )
})
Sys.sleep(1)


testthat::test_that("Pedestrian date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Pedestrian"
      )
    )
  )
})
Sys.sleep(1)


testthat::test_that("Bus date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Bus"
      )
    )
  )
})
Sys.sleep(1)

testthat::test_that("Rail date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Rail"
      )
    )
  )
})
