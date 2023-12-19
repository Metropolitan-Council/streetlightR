test_that("review status check", {

  check_analysis_review(analysis_name = "TC Parks Mem to Labor, 2017 w/attributes Blcks") %>% 
    testthat::expect_warning()
  
  })




test_that("review status check", {
  
  Sys.sleep(6)
  
  check_analysis_review(analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr") %>% 
    testthat::expect_warning()
  
})

