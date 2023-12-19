test_that("correct values returned on Pt. Douglass", {

  Sys.sleep(4)
  
  lookup_analysis_id(analysis_name = "Pt Douglas Regional Parking Lot Summer 22") %>% 
    expect_equal("89ce3700-872e-40d5-981b-019fe0663884")
  
  Sys.sleep(4)
  
  lookup_analysis_id(uuid = "89ce3700-872e-40d5-981b-019fe0663884") %>% 
    expect_equal("Pt Douglas Regional Parking Lot Summer 22")
  })

test_that("correct values returned on VMT 2019", {
  
  Sys.sleep(4)
  
  lookup_analysis_id(analysis_name = "v231218-VMT 2019 month 3 Volume Trip Trav Attr") %>% 
    expect_equal("02cc3dd7-fd25-4825-ab54-0fb2b718becd")
  
  Sys.sleep(8)
  
  lookup_analysis_id(uuid = "02cc3dd7-fd25-4825-ab54-0fb2b718becd") %>% 
    expect_equal("v231218-VMT 2019 month 3 Volume Trip Trav Attr")
})
