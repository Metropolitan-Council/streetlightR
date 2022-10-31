test_that("correct class return", {
  streetlight_insight(
    key = httr2::secret_decrypt(
      "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
      key = "STREETLIGHTR_KEY"
    ),
    endpoint = "analyses"
  ) %>%
    testthat::expect_s3_class("httr2_request")
})

test_that("error on no key", {
  streetlight_insight(
    endpoint = "analyses"
  ) %>%
    testthat::expect_error()
})

test_that("error on bad endpoint", {
  streetlight_insight(
    key = httr2::secret_decrypt(
      "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
      key = "STREETLIGHTR_KEY"
    ),
    endpoint = "cats"
  ) %>%
    httr2::req_perform() %>%
    testthat::expect_error()
})
