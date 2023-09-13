requireNamespace("dplyr")
requireNamespace("ggplot2")
requireNamespace("dplyr")

# Load in the test data
beesFlagged <- BeeBDC::beesFlagged


# Visualise all flags for each dataSource (simplified to the text before the first underscore)
testOut <- plotFlagSummary(
  data = beesFlagged,
  # Colours in order of pass (TRUE), fail (FALSE), and NA
  flagColours = c("#127852", "#A7002D", "#BDBABB"),
  fileName = paste0("FlagsPlot_", Sys.Date(),".pdf"),
  outPath = tempdir(),
  width = 15, height = 9,
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", 'B. Mont.' = "BMont", 'B. Minkley' = "BMin", Ecd = "Ecd",
  Gaiarsa = "Gai", EPEL = "EPEL",
  returnPlot = TRUE
)


# Test the expected results
testthat::test_that("plotFlagSummary plot length match", {
  testthat::expect_equal(length(testOut), 9)
})

# Test classes
testthat::test_that("plotFlagSummary expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("plotFlagSummary expected class", {
  testthat::expect_equal(attributes(testOut)$class, c("gg","ggplot"))
})

# Check directory that the plot was saved
testthat::test_that("plotFlagSummary plot saved?", {
  testthat::expect_true(any(stringr::str_detect(list.files(tempdir()), "FlagsPlot_")))
})


