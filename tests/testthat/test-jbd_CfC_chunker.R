requireNamespace("rnaturalearth")
requireNamespace("BeeBDC")
requireNamespace("dplyr")

data("bees3sp")


countryOutput <- BeeBDC::jbd_CfC_chunker(data = bees3sp %>%
                                     tidyr::drop_na(decimalLatitude),
                                     lat = "decimalLatitude",
                                     lon = "decimalLongitude",
                                     country = "country",
                                     # How many rows to process at a time
                                     stepSize = 50,
                                     # Start row
                                     chunkStart = 1,
                                     path = tempdir(),
                                     scale = "medium", # Test at medium scale in this instance.
                                     append = FALSE)


  # Test expected number of rows
testthat::test_that("jbd_CfC_chunker row count expected", {
  testthat::expect_equal(nrow(countryOutput), 86)
})
  # Test output class
testthat::test_that("jbd_CfC_chunker expected class", {
  testthat::expect_type(countryOutput, "list")
})

# Test output class
testthat::test_that("jbd_CfC_chunker check that the input and output contries are the same — because nothing was changed for this test dataset.", {
  testthat::expect_equal(countryOutput$country, bees3sp %>%
                          tidyr::drop_na(decimalLatitude) %>%
                          dplyr::pull(country))
})






