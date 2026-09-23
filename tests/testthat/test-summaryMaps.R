
# summaryMaps() opens the default graphics device; use a null device so parallel
# workers don't create Rplots.pdf and trip the state inspector in other tests
withr::local_options(list(device = \(...) grDevices::pdf(NULL, ...)))

# If rnaturalearthdata is present, run tests
if(requireNamespace("rnaturalearthdata", quietly = TRUE)){
  # Load in example data
beesFlagged <- BeeBDC::beesFlagged


# Draw a global summary map for occurrence and species number by country
testMap <- BeeBDC::summaryMaps(
  data = beesFlagged,
  width = 10, height = 10,
  class_n = 3,
  class_Style = "fisher",
  outPath = tempdir(),
  fileName = "CountryMaps_fisher.pdf",
  returnPlot = TRUE,
  scale = 110
)


# Test class
   testthat::test_that("summaryMaps expected class", {
     testthat::expect_type(testMap, "object")
   })

   testthat::test_that("dupePlotR plot saved?", {
     testthat::expect_true(inherits(testMap, c("ggplot", "ggplot2::ggplot")))
   })


  # Test some internals
testthat::test_that("summaryMaps internals - testMap$layers[[1]]$geom_params$grob$name", {
  testthat::expect_equal(testMap$layers[[1]]$geom_params$grob$name, "layout")
})


} # END if require
