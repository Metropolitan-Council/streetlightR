suppressMessages(
  streetlightR::streetlight_api_key(
    key = httr2::secret_decrypt(
      "alpw45XhwAktRVyumqtE9-iw3l1gYACvdmlFMh-7oqmetzgEt92hdZHT18oilU8P",
      key = "STREETLIGHTR_KEY"
    )
  )
)

test_login <- httr2::secret_decrypt(
  "Q8yxP3atCs0CbbM_C3VBArmMQpNlcAPAlbRfNR_nBxVT0mcOyi7dLKxB",
  key = "STREETLIGHTR_KEY"
)
