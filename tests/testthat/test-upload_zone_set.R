
testthat::test_that("Correct warning returned", {
  example_polygon <- sf::st_sfc(
    sf::st_point(cbind(-93.09, 44.95)),
    crs = 4326
  ) %>%
    sf::st_buffer(20)

  upload_zone_set(
    login_email = httr2::secret_decrypt(
      "Q8yxP3atCs0CbbM_C3VBArmMQpNlcAPAlbRfNR_nBxVT0mcOyi7dLKxB",
      key = "STREETLIGHTR_KEY"
    ),
    key = httr2::secret_decrypt(
      "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
      key = "STREETLIGHTR_KEY"
    ),
    geom_type = "polygon",
    zones = example_polygon,
    zone_set_name = "testing"
  ) %>%
    testthat::expect_warning()
})
