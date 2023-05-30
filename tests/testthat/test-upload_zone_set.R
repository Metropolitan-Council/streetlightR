testthat::test_that("Correct warning returned", {
  example_polygon <- sf::st_sfc(
    sf::st_point(cbind(-93.09, 44.95)),
    crs = 4326
  ) %>%
    sf::st_buffer(20)

  upload_zone_set(
    login_email = test_login,
    geom_type = "polygon",
    zones = example_polygon,
    zone_set_name = "testing"
  ) %>%
    testthat::expect_warning()
})
