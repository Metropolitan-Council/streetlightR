Sys.sleep(2)

testthat::test_that("Shapefile is returned", {
  this_shp <- get_analysis_shapefile(
    analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
    shapefile = "zone_activity"
  ) %>% suppressWarnings()

  testthat::expect_equal(nrow(this_shp), 199)
  testthat::expect_equal(
    names(this_shp),
    c(
      "id", "name", "direction", "is_pass",
      "is_bidi", "geometry",
      "file_name", "shapefile"
    )
  )
})


Sys.sleep(2)

testthat::test_that("Shapefile is returned", {
  get_analysis_shapefile(
    analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
    shapefile = "zone_activity"
  ) %>%
    expect_error()
})