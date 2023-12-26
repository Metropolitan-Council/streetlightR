test_that("create tag testing", {
  create_tag(
    key = key,
    login_email = test_login,
    "streetlightR"
  ) %>%
    testthat::expect_warning("Tag names must be case-insensitive")
})
