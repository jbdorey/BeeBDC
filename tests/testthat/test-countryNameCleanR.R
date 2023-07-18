requireNamespace("readr")
requireNamespace("dplyr")
library(dplyr) ## could not use %>% without loading as library


# make yourself some test data which has common problem names for the USA and one row that is already correct
testData <- dplyr::tribble(
              ~database_id, ~decimalLatitude, ~decimalLongitude,                  ~country, ~countryCode,                 ~scientificName, ~dataSource, ~recordedBy,        ~order,  ~family, ~genus, ~specificEpithet, ~scientificNameAuthorship,
               "fake SCAN",            38.54,        -121.75667,           "United States",           NA, "Apis mellifera Linneaus, 1758",      "fake",          NA, "Hymenoptera", "apidae", "Apis",      "mellifera",          "Linnaeus, 1758",
               "fake SCAN",            38.54,        -121.75667,                  "U.S.A.",           NA, "Apis mellifera Linneaus, 1759",      "fake",          NA, "Hymenoptera", "apidae", "Apis",      "mellifera",          "Linnaeus, 1758",
               "fake SCAN",            38.54,        -121.75667,                  "U.S.A.",           NA, "Apis mellifera Linneaus, 1760",      "fake",          NA, "Hymenoptera", "apidae", "Apis",      "mellifera",          "Linnaeus, 1758",
               "fake SCAN",            38.54,        -121.75667,                      "US",           NA, "Apis mellifera Linneaus, 1761",      "fake",          NA, "Hymenoptera", "apidae", "Apis",      "mellifera",          "Linnaeus, 1758",
               "fake SCAN",            38.54,        -121.75667,                     "USA",           NA, "Apis mellifera Linneaus, 1762",      "fake",          NA, "Hymenoptera", "apidae", "Apis",      "mellifera",          "Linnaeus, 1758",
               "fake SCAN",            38.54,        -121.75667,                     "usa",           NA, "Apis mellifera Linneaus, 1763",      "fake",          NA, "Hymenoptera", "apidae", "Apis",      "mellifera",          "Linnaeus, 1758",
               "fake SCAN",            38.54,        -121.75667,           "UNITED STATES",           NA, "Apis mellifera Linneaus, 1764",      "fake",          NA, "Hymenoptera", "apidae", "Apis",      "mellifera",          "Linnaeus, 1758",
               "fake SCAN",            38.54,        -121.75667,"United States of America",           NA, "Apis mellifera Linneaus, 1758",      "fake",          NA, "Hymenoptera", "apidae", "Apis",      "mellifera",          "Linnaeus, 1758",
               "fake SCAN",            38.54,        -121.75667,                     "usa",           NA, "Apis mellifera Linneaus, 1765",      "fake",          NA, "Hymenoptera", "apidae", "Apis",      "mellifera",          "Linnaeus, 1758"
              )

# common problem table from the function documentation
commonProblems <- dplyr::tibble(problem = c('U.S.A.', 'US','USA','usa','UNITED STATES',
                                            'United States','U.S.A','MX','CA','Bras.','Braz.',
                                            'Brasil','CNMI','USA TERRITORY: PUERTO RICO'),
                                 fix = c('United States of America','United States of America',
                                        'United States of America','United States of America',
                                        'United States of America','United States of America',
                                        'United States of America','Mexico','Canada','Brazil',
                                        'Brazil','Brazil','Northern Mariana Islands','PUERTO.RICO'))


# run the function!
testOut <- BeeBDC::countryNameCleanR(data = testData, commonProblems = commonProblems)


# check that country column has only one value (correct value)
numberOfCountries <- testOut %>% group_by(country) %>% summarise()


# number of NA values in fix column
numberNAs <- sum(is.na(testOut$fix))


# Test the number of expected fixed and NA values in the fix column and
# verify addition of new column
testthat::test_that("number of country names in the fixed country column" , {
  testthat::expect_equal(ncol(numberOfCountries), 1)
})

testthat::test_that("number of unchanged rows in data", {
  testthat::expect_equal(numberNAs, 1)
})




