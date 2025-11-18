test_that("search for OSM ids", {
  
  example_polygon <- sf::st_sfc(
    sf::st_point(cbind(-93.09, 44.95)),
    crs = 4326
  ) %>%
    sf::st_buffer(20)
  
  polygon_response <- search_osm_ids(polygon = example_polygon)
  
  point_response <- search_osm_ids(point = c(-93.09, 44.95),
                                   radius = 0.5)
  
  point_response <- search_osm_ids(point = sf::st_sfc(sf::st_point(cbind(-93.09, 44.95)), crs = 4326),
                                   radius = 0.5)
  
  
  zip_response <- search_osm_ids(zip_id = "55104")
  tract_response <- search_osm_ids(tract_id = "27053023000")
  bg_response <- search_osm_ids(blockgroup_id = "270530256031")
  
  testthat::expect_error(search_osm_ids(tract_id = "235.02"))
  testthat::expect_error(search_osm_ids(zip_id  = "5501"))
})
