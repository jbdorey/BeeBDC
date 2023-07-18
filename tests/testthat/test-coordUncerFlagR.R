requireNamespace("readr")
requireNamespace("BeeBDC")


# make us some data - this is FAKE DATA derived from records from SCAN
testData <- dplyr::tribble(
                   ~basisOfRecord,   ~kingdom,      ~phylum,    ~class,        ~order,  ~family,  ~scientificName, ~taxonID, ~scientificNameAuthorship, ~genus, ~specificEpithet, ~recordedBy, ~eventDate, ~year, ~month, ~day,        ~verbatimEventDate,        ~country, ~stateProvince, ~locality,                  ~locationRemarks, ~decimalLatitude, ~decimalLongitude, ~coordinateUncertaintyInMeters,                                             ~rights,                                     ~rightsHolder,
              "PreservedSpecimen", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera", "P.S. Ward",  "6/28/05", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005", "United States",   "California",   "Davis", "coordinates obtained from Label",            38.54,        -121.75667,                           200L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",
              "PreservedSpecimen", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera", "P.S. Ward",  "6/28/05", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005", "United States",   "California",   "Davis", "coordinates obtained from Label",            38.54,        -121.75667,                            10L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",
              "PreservedSpecimen", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera", "P.S. Ward",  "6/28/05", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005", "United States",   "California",   "Davis", "coordinates obtained from Label",            38.54,        -121.75667,                           500L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",
              "PreservedSpecimen", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera", "P.S. Ward",  "6/28/05", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005", "United States",   "California",   "Davis", "coordinates obtained from Label",            38.54,        -121.75667,                          1000L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",
              "PreservedSpecimen", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera", "P.S. Ward",  "6/28/05", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005", "United States",   "California",   "Davis", "coordinates obtained from Label",            38.54,        -121.75667,                           850L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",
              "PreservedSpecimen", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera", "P.S. Ward",  "6/28/05", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005", "United States",   "California",   "Davis", "coordinates obtained from Label",            38.54,        -121.75667,                           600L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",
              "PreservedSpecimen", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera", "P.S. Ward",  "6/28/05", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005", "United States",   "California",   "Davis", "coordinates obtained from Label",            38.54,        -121.75667,                           150L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org",
              "PreservedSpecimen", "Animalia", "Arthropoda", "Insecta", "Hymenoptera", "apidae", "apis mellifera",  235783L,          "Linnaeus, 1758", "Apis",      "mellifera", "P.S. Ward",  "6/28/05", 2005L,     6L,  28L, "28 Jun 2005/29 Jun 2005", "United States",   "California",   "Davis", "coordinates obtained from Label",            38.54,        -121.75667,                            20L, "http://creativecommons.org/publicdomain/zero/1.0/", "The California Academy of Sciences - AntWeb.org"
              )


# run the function
testOut <- BeeBDC::coordUncerFlagR(data = testData, uncerColumn = "coordinateUncertaintyInMeters", threshold = 500)


# number of FALSE values in new column, .uncertaintyThreshold
resultsT <- length(testOut$.uncertaintyThreshold[testOut$.uncertaintyThreshold == TRUE])
resultsF <- length(testOut$.uncertaintyThreshold[testOut$.uncertaintyThreshold == FALSE])


# number of columns in old and new data
old <- ncol(testData)
new <- ncol(testOut)
testcols <- new - old


# Test the number of expected TRUE and FALSE values in the .uncertaintyThreshold column and
# verify addition of new column
testthat::test_that("coordUncerFlagR results columns TRUE", {
  testthat::expect_equal(resultsT, 5)
})

testthat::test_that("coordUncerFlagR results columns FALSE", {
  testthat::expect_equal(resultsF, 3)
})

testthat::test_that("verify addition of new column", {
  testthat::expect_equal(testcols, 1)
})

