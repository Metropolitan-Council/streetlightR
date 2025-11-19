test_names <- function(x) {
  x$data %>%
    names() %>%
    testthat::expect_equal(c("osm_id", "road_classification", "osm_segment_count"))
}


test_that("search for OSM ids", {
  example_polygon <- sf::st_sfc(
    sf::st_point(cbind(-93.09, 44.95)),
    crs = 4326
  ) %>%
    sf::st_buffer(20)

  polygon_response <- search_osm_ids(polygon = example_polygon)

  Sys.sleep(2)

  point_response_list <- search_osm_ids(
    point = c(-93.09, 44.95),
    radius = 0.5
  )
  Sys.sleep(2)

  point_response_point <- search_osm_ids(
    point = sf::st_sfc(sf::st_point(
      cbind(-93.09, 44.95)
    ), crs = 4326),
    radius = 0.5
  )

  Sys.sleep(2)

  zip_response <- search_osm_ids(zip_id = "55104")

  Sys.sleep(2)

  tract_response <- search_osm_ids(tract_id = "27053023000")

  Sys.sleep(2)

  bg_response <- search_osm_ids(blockgroup_id = "270530256031")

  Sys.sleep(2)


  testthat::expect_error(search_osm_ids(tract_id = "235.02"))
  testthat::expect_error(search_osm_ids(zip_id = "5501"))


  test_names <- function(x) {
    # browser()
    x$data %>%
      names() %>%
      testthat::expect_equal(c("osm_id", "road_classification", "osm_segment_count"))
  }


  purrr::map(
    list(
      point_response_list,
      point_response_point,
      polygon_response,
      zip_response,
      bg_response,
      tract_response
    ),
    test_names
  )
})

test_that("search for OSM ids - zip", {
  responses <- purrr::map(
    list(
      "55105",
      "55101",
      "55113",
      "56357",
      "61759"
    ),
    function(x) {
      Sys.sleep(2)
      do.call(search_osm_ids, args = list("zip_id" = x)) %>%
        return()
    }
  )

  purrr::map(
    responses,
    test_names
  )
})

test_that("search for OSM ids - block group", {
  responses <- purrr::map(
    list(
      "271630712093",
      "270370610032",
      "270359512002",
      "551091204013",
      "190594505021"
    ),
    function(x) {
      Sys.sleep(2)
      do.call(search_osm_ids, args = list("blockgroup_id" = x)) %>%
        return()
    }
  )

  purrr::map(
    responses,
    test_names
  )
})


test_that("search for OSM ids - tract", {
  responses <- purrr::map(
    list(
      "19059450502",
      "27019090702",
      "27053027205",
      "55019950400",
      "19009070200"
    ),
    function(x) {
      Sys.sleep(2)
      do.call(search_osm_ids, args = list("tract_id" = x)) %>%
        return()
    }
  )

  purrr::map(
    responses,
    test_names
  )
})
