requireNamespace("dplyr")


# Load in a test dataset
beesCountrySubset <- BeeBDC::beesCountrySubset

testOut <- BeeBDC::diversityPrepR(
  data = beesCountrySubset,
  # Download the taxonomy
  taxonomyFile = BeeBDC::beesTaxonomy(),
  # Download the checklist
  checklistFile = BeeBDC::beesChecklist(),
  curveFunction = function(x) (228.7531 * x * x^-log(12.1593)),
  sampleSize = 10000,
  countryColumn = "country_suggested",
  limitGlobal = NULL,
  outPath = tempdir()
)


# Test the expected results
#testthat::test_that("dateFindR results successfuly matched", {
#  testthat::expect_equal(sum(complete.cases(testOut$eventDate) ), 23)
#})
#testthat::test_that("dateFindR results unsuccessfuly matched", {
#  testthat::expect_equal(sum(is.na(testOut$eventDate) ), 1)
#})
