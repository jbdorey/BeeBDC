requireNamespace("rnaturalearth")
requireNamespace("BeeBDC")
requireNamespace("dplyr")

# If rnaturalearthdata is present, run tests
if(requireNamespace("rnaturalearthdata")){

bees3sp <- BeeBDC::bees3sp

# Set up some fake data
fakeData <- dplyr::tibble(
  database_id = c("a1","a2","a3","a4", "a5","b1", "b2") ,
  decimalLatitude = c(-17.556329, 17.556329, 17.556329, 63.43333, -17.90000,-28.3497, 30.556553),
  decimalLongitude = c(178.007245, 178.007245, 178.007245, -17.90000, 63.43333, 114.6328, 108.832062),
  country = c("", "", "", "", "","",""),
  countryCode = c("FJ", "FJ", "US", "BO", "BO","AU","CH"),
  scientificName = c("sp1","sp1","sp1","sp1", "sp2","sp3","sp4")
)


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
                                     scale = "medium")


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


# Do a second round of test with fail data
countryOutput <- BeeBDC::jbd_CfC_chunker(data = bees3sp %>%
                                           tidyr::drop_na(decimalLatitude) %>%
                                           dplyr::bind_rows(fakeData),
                                         lat = "decimalLatitude",
                                         lon = "decimalLongitude",
                                         country = "country",
                                         # How many rows to process at a time
                                         stepSize = 50,
                                         # Start row
                                         chunkStart = 1,
                                         path = tempdir(),
                                         scale = "medium")


# Test expected number of rows
testthat::test_that("jbd_CfC_chunker row count expected", {
  testthat::expect_equal(nrow(countryOutput), 89) # Three extra rows from the fake data
})
# Test output class
testthat::test_that("jbd_CfC_chunker expected class", {
  testthat::expect_type(countryOutput, "list")
})

# Test output class
testthat::test_that("jbd_CfC_chunker check that the input and output contries are the same — because nothing was changed for this test dataset.", {
  testthat::expect_equal(countryOutput$country, c(bees3sp %>%
                           tidyr::drop_na(decimalLatitude) %>%
                           dplyr::pull(country), "Fiji", "Australia", "China"))
})



#     # Do a third round of test with fail data and multiple cores
#     countryOutput <- BeeBDC::jbd_CfC_chunker(data = bees3sp %>%
#                                                tidyr::drop_na(decimalLatitude) %>%
#                                                dplyr::bind_rows(fakeData),
#                                              lat = "decimalLatitude",
#                                              lon = "decimalLongitude",
#                                              country = "country",
#                                              # How many rows to process at a time
#                                              stepSize = 50,
#                                              # Start row
#                                              chunkStart = 1,
#                                              path = tempdir(),
#                                              scale = "medium", # Test at medium scale in this instance.
#                                              mc.cores = 2)
#     
#     
#     # Test expected number of rows
#     testthat::test_that("jbd_CfC_chunker row count expected", {
#       testthat::expect_equal(nrow(countryOutput), 89) # Three extra rows from the fake data
#     })
#     # Test output class
#     testthat::test_that("jbd_CfC_chunker expected class", {
#       testthat::expect_type(countryOutput, "list")
#     })
#     
#     # Test output class
#     testthat::test_that("jbd_CfC_chunker check that the input and output contries are the same — because nothing was changed for this test dataset.", {
#       testthat::expect_equal(countryOutput$country, c(bees3sp %>%
#                                                         tidyr::drop_na(decimalLatitude) %>%
#                                                         dplyr::pull(country), "Fiji", "Australia", "China"))
#     })
#     
#     


} # END if require

