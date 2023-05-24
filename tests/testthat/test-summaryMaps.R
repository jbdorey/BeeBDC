data("beesFlagged")




# Draw a global summary map for occurrence and species number by country
testMap <- BeeDC::summaryMaps(
  mapData = beesFlagged,
  width = 10, height = 10,
  class_n = 3,
  class_Style = "fisher",
  filename = paste0(tempdir(), "/CountryMaps_fisher.pdf", sep = ""),
  returnPlot = TRUE
)


# Test class
testthat::test_that("summaryMaps expected class", {
  testthat::expect_type(testMap, "list")
})
  # Test list size
testthat::test_that("summaryMaps list size", {
  testthat::expect_equal(testMap, 9)
})
  # Test some internals
testthat::test_that("summaryMaps internals - testMap$layers[[1]]$geom_params$grob$name", {
  testthat::expect_equal(testMap$layers[[1]]$geom_params$grob$name, "layout")
})
