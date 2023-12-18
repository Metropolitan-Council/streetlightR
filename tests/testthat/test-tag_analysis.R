test_that("add tags to analysis", {

  # test "v231218-VMT 2019 month 3 Volume Trip Trav Attr"
  tag_analysis(
    key = key,
    login_email = test_login,
    analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
    tag_name = "streetlightR"
  ) %>% 
    testthat::expect_message("Add tag succeeded with message: success")
  
  Sys.sleep(1)
  
  tag_analysis(
    key = key,
    login_email = test_login,
    analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
    tag_name = "liz-test-cats"
  ) %>% 
    testthat::expect_warning()
  
  Sys.sleep(1)
  
  # test "Pt Douglas Regional Trail Ped Summer 19-21"
  tag_analysis(
    key = key,
    login_email = test_login,
    analysis_name = "Pt Douglas Regional Trail Ped Summer 19-21",
    tag_name = "testing-tag"
  ) %>% 
    testthat::expect_message()
  
  
  Sys.sleep(1)
  
  tag_analysis(
    key = key,
    login_email = test_login,
    analysis_name = "Pt Douglas Regional Trail Ped Summer 19-21",
    tag_name = "testing-tag"
  ) %>% 
    testthat::expect_message("Add tag succeeded with message: success")
  
  })


test_that("remove tags from analysis", {
  
  Sys.sleep(1)
  
  # test "v231218-VMT 2019 month 3 Volume Trip Trav Attr"
  remove_analysis_tag(
    key = key,
    login_email = test_login,
    analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
    tag_name = "streetlightR"
  ) %>% 
    testthat::expect_message("Remove tag succeeded with message: success")
  
  Sys.sleep(1)
  
  remove_analysis_tag(
    key = key,
    login_email = test_login,
    analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr",
    tag_name = "liz-test-cats"
  ) %>% 
    testthat::expect_warning()
  
  Sys.sleep(1)
  
  # test "Pt Douglas Regional Trail Ped Summer 19-21"
  remove_analysis_tag(
    key = key,
    login_email = test_login,
    analysis_name = "Pt Douglas Regional Trail Ped Summer 19-21",
    tag_name = "testing-tag"
  ) %>% 
    testthat::expect_message()
  
  Sys.sleep(1)
  
  remove_analysis_tag(
    key = key,
    login_email = test_login,
    analysis_name = "Pt Douglas Regional Trail Ped Summer 19-21",
    tag_name = "testing-tag"
  ) %>% 
    testthat::expect_message("Remove tag succeeded with message: success")
  
})
