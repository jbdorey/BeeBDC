
# If rnaturalearthdata is present, run tests
if(requireNamespace("rnaturalearthdata")){
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
  testthat::expect_type(testMap, "list")
})
testthat::test_that("summaryMaps expected class", {
  testthat::expect_true(any(stringr::str_detect(attributes(testMap)$class, "gg|ggplot")))
})


  # Test some internals
testthat::test_that("summaryMaps internals - testMap$layers[[1]]$geom_params$grob$name", {
  testthat::expect_equal(testMap$layers[[1]]$geom_params$grob$name, "layout")
})


} # END if require


