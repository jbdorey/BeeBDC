requireNamespace("dplyr")
skip_if_not_installed("rnaturalearthdata")

# Load a test dataset
beesRaw <- BeeBDC::beesRaw
# Load the small testTaxonomy file
system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |>
  load()
# Load the small checklist file
system.file("extdata", "testChecklist.rda", package="BeeBDC") |>
  load()

testOut <- richnessPrepR(
  data = beesRaw,
  # Download the taxonomy
  taxonomyFile = testTaxonomy,
  # Download the checklist
  checklistFile = testChecklist,
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

