requireNamespace("dplyr")



testData <- dplyr::tribble( # even step coordinates (different for lat/long)
  ~database_id, ~datasetName,  ~fieldNotes,                             ~id, ~institutionCode, ~collectionCode,         ~ownerInstitutionCode,      ~basisOfRecord,                  ~occurrenceID,      ~catalogNumber, ~otherCatalogNumbers,   ~kingdom,      ~phylum,    ~class,        ~order,  ~family,  ~scientificName, ~taxonID, ~scientificNameAuthorship, ~genus, ~specificEpithet,        ~recordedBy, ~eventDate, ~year,       ~month, ~day,  ~verbatimEventDate,        ~country, ~stateProvince,  ~locality,                  ~locationRemarks, ~decimalLatitude, ~decimalLongitude, ~minimumElevationInMeters,                                             ~rights,                                     ~rightsHolder, ~accessRights,                                       ~recordId,                                                                        ~references,
  "fake SCAN1", "fakeDataset", "",                                      13775122L,            "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen",     "CAS:ANTWEB:casent0106100",     "casent0106100",                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "",        NA,        NA,  NA, "", "United States",   "California",    "Davis", "coordinates obtained from Label; 6/28/05",           38.541,        -121.75667,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA, "urn:uuid:46a46727-6535-4e70-88e7-a42c98f806ed", "https://scan-bugs.org:443/portal/collections/individual/index.php?occid=13775122",
  "fake SCAN2", "fakeDataset", "",                                      13775123L,            "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen", "CAS:ANTWEB:casent0106100-d01", "casent0106100-d01",                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "",        NA,        NA,  NA, "", "United States",   "California",    "Davis", "coordinates obtained from Label: 3 march 2022",           38.542,        -121.75668,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA, "urn:uuid:d6ff3ddb-4695-4aaf-ab89-c251ab2fc7e6", "https://scan-bugs.org:443/portal/collections/individual/index.php?occid=13775123",
  "fake SCAN3", "fakeDataset", "28 Jun 2005",               13775124L,            "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen", "CAS:ANTWEB:casent0106100-d11", "casent0106100-d11",                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "",        NA,        NA,  NA, "", "United States",   "California",    "Davis", "coordinates obtained from Label",           38.543,        -121.75669,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA, "urn:uuid:4504a6e6-ab1c-4de7-ac43-a3588a63ae90", "https://scan-bugs.org:443/portal/collections/individual/index.php?occid=13775124",
  "fake SCAN4", "fakeDataset", "",                                      NA,                   "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen",                             NA,                  NA,                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "",        2022,        2,  1, "", "United States",   "California",    "Davis", "coordinates obtained from Label",           38.544,        -121.75670,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA,                                              NA,                                                                                 NA,
  "fake SCAN5", "fakeDataset", "",                                      NA,                   "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen",                             NA,                  NA,                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "",        NA,     NA,  NA, "28 Jun 2005/29 Jun 2005", "United States",   "California",    "Davis", "coordinates obtained from Label",           38.545,        -121.75671,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA,                                              NA,                                                                                 NA,
  "fake SCAN6", "fakeDataset", "",                                      NA,                   "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen",                             NA,                  NA,                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005", "United States",   "California",    "Davis", "coordinates obtained from Label",           38.546,        -121.75672,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA,                                              NA,                                                                                 NA,
  "fake SCAN7", "fakeDataset", "",                                      NA,                   "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen",                             NA,                  NA,                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "", NA,     NA,  NA, "28 IX 2005", "United States",   "California",    "Davis", "coordinates obtained from Label",           38.546,        -121.75672,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA,                                              NA,                                                                                 NA,
  "fake SCAN8", "fakeDataset", "",                                      NA,                   "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen",                             NA,                  NA,                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "", NA,     NA,  NA, "I 2022", "United States",   "California",    "Davis", "coordinates obtained from Label",           38.546,        -121.75672,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA,                                              NA,                                                                                 NA,
  "fake SCAN10", "fakeDataset", "",                                      NA,                   "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen",                             NA,                  NA,                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "", NA,     NA,  NA, "2022", "United States",   "California",    "Davis", "coordinates obtained from Label",           38.546,        -121.75672,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA,                                              NA,                                                                                 NA,
  "fake SCAN11", "fakeDataset", "",                                      NA,                   "CAS",        "ANTWEB",        "UCDC, Davis, CA, USA", "PreservedSpecimen",                             NA,                  NA,                   NA, "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera",        "P.S. Ward",  "2005-05-05", NA,     NA,  NA, "2022", "United States",   "California",    "Davis", "coordinates obtained from Label",           38.546,        -121.75672,                       15L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",            NA,                                              NA,                                                                                 NA
  
  )


testOut <- BeeBDC::dateFindR(data = testData,
                        # Years above this are removed (from the recovered dates only)
                        maxYear = lubridate::year(Sys.Date()),
                        # Years below this are removed (from the recovered dates only)
                        minYear = 1700) %>%
  dplyr::select(database_id, eventDate, fieldNotes, locationRemarks, verbatimEventDate, 
                day, month, year)


# Test the expected results
testthat::test_that("dateFindR results successfuly matched", {
  testthat::expect_equal(sum(complete.cases(testOut$eventDate) ), 9)
})
testthat::test_that("dateFindR results unsuccessfuly matched", {
  testthat::expect_equal(sum(is.na(testOut$eventDate) ), 1)
})

testthat::test_that("dateFindR output dates match", {
  testthat::expect_equal(testOut$eventDate %>% as.character(),
                         c("2005-06-28", "2022-03-03", "2005-06-28", "2022-02-01", 
                           "2005-06-28", "2005-06-28", "2020-09-28", "2022-01-01",
                          NA, "2005-05-05"))
})

  # Test classes
testthat::test_that("dateFindR expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("dateFindR expected class", {
  testthat::expect_equal(attributes(testOut)$class, c("tbl_df","tbl" ,"data.frame"))
})




