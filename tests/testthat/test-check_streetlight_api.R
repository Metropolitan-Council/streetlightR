Sys.sleep(1)
testthat::test_that("API key test retuns value", {
  testthat::expect_equal(
    check_streetlight_api(
      key = httr2::secret_decrypt(
        "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
        key = "STREETLIGHTR_KEY"
      )
    ),
    "PASS"
  )
})
