suppressMessages(
  streetlightR::streetlight_api_key(
    key = httr2::secret_decrypt(
      "Zj4CCBvezhp3MoouyMxfmO7Htc6AVlwkBIB1va7ISjXOXS8PfexLOGCZiE5_wQFu",
      key = "STREETLIGHTR_KEY"
    )
  )
)

test_login <- httr2::secret_decrypt(
  "Q8yxP3atCs0CbbM_C3VBArmMQpNlcAPAlbRfNR_nBxVT0mcOyi7dLKxB",
  key = "STREETLIGHTR_KEY"
)

