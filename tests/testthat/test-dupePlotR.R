requireNamespace("dplyr")
requireNamespace("ggplot2")
requireNamespace("BeeBDC")


beesFlagged <- BeeBDC::beesFlagged


# Create a figure showing the total number of duplicates, kept duplicates, and unique
# records for each datasource (simplified to the text before the first underscore) and
# the proportion of the above for each data source
testOut <- BeeBDC::dupePlotR(
  data = beesFlagged,
  # The outPath to save the plot as
  outPath = tempdir(),
  fileName = "duplicatePlot.pdf",
  # Colours in order: duplicate, kept duplicate, unique
  dupeColours = c("#F2D2A2","#B9D6BC", "#349B90"),
  # Plot size and height
  base_height = 7, base_width = 7,
  legend.position = c(0.85, 0.8),
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", 
  returnPlot = TRUE
)


# Test the expected results
    # testthat::test_that("dupePlotR plot length match", {
    #   testthat::expect_equal(length(testOut), 9)
    # })

# Test classes
testthat::test_that("dupePlotR expected class", {
  testthat::expect_type(testOut, "list")
})
testthat::test_that("dupePlotR expected class", {
  testthat::expect_equal(attributes(testOut)$class, c("gg","ggplot"))
})

  # Check directory that the plot was saved
testthat::test_that("dupePlotR plot saved?", {
  testthat::expect_true(any(stringr::str_detect(list.files(tempdir()), "duplicatePlot.pdf")))
})

