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
    zone_set_name = "testing-today",
    with_calibration = TRUE
  ) %>%
    testthat::expect_warning()


  Sys.sleep(3)

  upload_zone_set(
    login_email = test_login,
    geom_type = "polygon",
    zones = example_polygon,
    zone_set_name = "testing-today",
    with_calibration = FALSE
  ) %>%
    testthat::expect_warning()
})

testthat::test_that("OSM set created", {
  example_polygon <- sf::st_sfc(
    sf::st_point(cbind(-93.09, 44.95)),
    crs = 4326
  ) %>%
    sf::st_buffer(50)

  polygon_response <- search_osm_ids(polygon = example_polygon)

  Sys.sleep(3)


  create_zone_set(
    login_email = test_login,
    osm_ids = list(polygon_response$data$osm_id) %>% unlist(),
    zone_set_name = paste0("streetlightR-test-osm_upl5_", Sys.Date())
  ) %>% 
    testthat::expect_no_error()
})
