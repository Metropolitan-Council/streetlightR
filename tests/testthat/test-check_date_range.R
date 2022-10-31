testthat::test_that("Default travel mode date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        key = httr2::secret_decrypt(
          "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
          key = "STREETLIGHTR_KEY"
        )
      )
    )
  )
})

Sys.sleep(1)

testthat::test_that("Bike date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Bicycle",
        key = httr2::secret_decrypt(
          "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
          key = "STREETLIGHTR_KEY"
        )
      )
    )
  )
})

Sys.sleep(1)

testthat::test_that("Truck date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Truck",
        key = httr2::secret_decrypt(
          "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
          key = "STREETLIGHTR_KEY"
        )
      )
    )
  )
})
Sys.sleep(1)


testthat::test_that("Pedestrian date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Pedestrian",
        key = httr2::secret_decrypt(
          "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
          key = "STREETLIGHTR_KEY"
        )
      )
    )
  )
})
Sys.sleep(1)


testthat::test_that("Bus date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Bus",
        key = httr2::secret_decrypt(
          "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
          key = "STREETLIGHTR_KEY"
        )
      )
    )
  )
})
Sys.sleep(1)

testthat::test_that("Rail date range returns table", {
  testthat::expect_true(
    tibble::is_tibble(
      check_date_range(
        travel_mode_type = "Rail",
        key = httr2::secret_decrypt(
          "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
          key = "STREETLIGHTR_KEY"
        )
      )
    )
  )
})
