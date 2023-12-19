Sys.sleep(1)
testthat::test_that("API key test retuns value", {
  testthat::expect_equal(
    check_streetlight_api(
      key = httr2::secret_decrypt(
        "alpw45XhwAktRVyumqtE9-iw3l1gYACvdmlFMh-7oqmetzgEt92hdZHT18oilU8P",
        key = "STREETLIGHTR_KEY"
      )
    ),
    "PASS"
  )
})
