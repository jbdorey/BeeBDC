requireNamespace("dplyr")


# Load in a test dataset
beesCountrySubset <- BeeBDC::beesCountrySubset
taxonomyFile <- BeeBDC::beesTaxonomy()
checklistFile <- BeeBDC::beesChecklist()

testOut <- richnessPrepR(
  data = beesCountrySubset,
  # Download the taxonomy
  taxonomyFile = taxonomyFile,
  # Download the checklist
  checklistFile = checklistFile,
  curveFunction = function(x) (228.7531 * x * x^-log(12.1593)),
  sampleSize = 10000,
  countryColumn = "country_suggested",
  limitGlobal = NULL,
  outPath = tempdir()
)


# Test the expected results
testthat::test_that("richnessPrepR results successfuly matched", {
  testthat::expect_equal(length(testOut) , 10)
})
testthat::test_that("richnessPrepR names match", {
  testthat::expect_equal(names(testOut) ,
                         c("curveExtraction","checklistFile","taxonomyFile","taxonomyAcceptedNames",
                           "taxonomyNoPoints","data_counts","continentChecklist","continentNoPoints",
                           "worldMap","siteSpeciesCounts"    
                           ) )
})

