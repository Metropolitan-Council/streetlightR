test_that("list available tags", {
  tags <- list_available_tags()

  testthat::expect_true(tibble::is_tibble(tags))
  testthat::expect_equal(
    names(tags),
    c(
      "analysis_num", "created_by",
      "created_date", "tag_name"
    )
  )
  testthat::expect_gt(nrow(tags), 400)
})
