testthat::test_that("Shapefile is returned", {
  try_again(times = 5, {
    Sys.sleep(3)
    this_shp <- get_analysis_shapefile(
      analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
      shapefile = "zone_activity"
    ) %>% suppressWarnings()
  })

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

testthat::test_that("Rate limit", {
  get_analysis_shapefile(
    analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
    shapefile = "zone_activity"
  ) %>%
    expect_error()
})
