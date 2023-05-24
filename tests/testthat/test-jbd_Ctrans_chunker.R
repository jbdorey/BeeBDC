requireNamespace("dplyr")
data("bees3sp")

testOut <- BeeDC::jbd_Ctrans_chunker(
  # bdc_coordinates_transposed inputs
  data = bees3sp %>%
    dplyr::filter(complete.cases(decimalLatitude)) %>%
    dplyr::select(!c(".sea", ".val")) %>%
    dplyr::bind_rows(bees3sp %>%  dplyr::select(!c(".sea", ".val")) %>% 
                       dplyr::filter(complete.cases(decimalLatitude)) %>% dplyr::slice_head(n = 20) %>%
                        # Swap the first ten rows' lat and lon and then add to the dataset to test the function
                       dplyr::mutate(decimalLatitude2 = decimalLongitude,
                                     decimalLongitude2 = decimalLatitude,
                                     decimalLongitude = decimalLongitude2,
                                     decimalLatitude = decimalLatitude2)),
  id = "database_id",
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country",
  countryCode = "countryCode",
  border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
  save_outputs = TRUE,
  sci_names = "scientificName",
  # chunker inputs
  stepSize = 55,  # How many rows to process at a time
  chunkStart = 1,  # Start row
  append = FALSE  # If FALSE it may overwrite existing dataset
) 

# Get a count of TRUE and FALSE column name matches
resultsT <- sum(testOut$coordinates_transposed == TRUE)
resultsF <- sum(testOut$coordinates_transposed == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("jbd_Ctrans_chunker results TRUE", {
  testthat::expect_equal(resultsT, 104)
})
testthat::test_that("jbd_Ctrans_chunker results FALSE", {
  testthat::expect_equal(resultsF, 2)
})


# Test expected number of rows
testthat::test_that("jbd_Ctrans_chunker row count expected", {
  testthat::expect_equal(nrow(testOut), 106)
})
# Test output class
testthat::test_that("jbd_Ctrans_chunker expected class", {
  testthat::expect_type(testOut, "list")
})



