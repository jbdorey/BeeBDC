requireNamespace("dplyr")
bees3sp <- BeeBDC::bees3sp

fakeData <- dplyr::tibble(
  database_id = c("a1","a2","a3","a4", "a5") ,
  decimalLatitude = c(-17.556329, 17.556329, 17.556329, 63.43333, -17.90000),
  decimalLongitude = c(178.007245, 178.007245, 178.007245, -17.90000, 63.43333),
  country = c("Fiji", "Fiji", "United States", "Bolivia", "Bolivia"),
  countryCode = c("FJ", "FJ", "US", "BO", "BO"),
  scientificName = c("sp1","sp1","sp1","sp1", "sp2")
)

testOut <- BeeBDC::jbd_Ctrans_chunker(
  # bdc_coordinates_transposed inputs
  data = bees3sp %>%
    dplyr::filter(complete.cases(decimalLatitude)) %>%
    dplyr::select(c(database_id, decimalLatitude, decimalLongitude, country, countryCode,
                    scientificName)) %>%
    dplyr::filter(dplyr::row_number() %in% 1:20) %>%
    dplyr::bind_rows(fakeData),
  idcol = "database_id",
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country",
  countryCode = "countryCode",
    # Larger buffer to speed up operation for test
  border_buffer = 0.9, # in decimal degrees (~22 km at the equator)
  save_outputs = FALSE,
  sci_names = "scientificName",
  # chunker inputs
  stepSize = 100,  # How many rows to process at a time
  chunkStart = 1,  # Start row
  path = tempdir(),
  append = FALSE,  # If FALSE it may overwrite existing dataset
  progressiveSave = FALSE,
  scale = "medium",
  mc.cores = 1
) 

# Get a count of TRUE and FALSE column name matches
resultsT <- sum(testOut$coordinates_transposed == TRUE)
resultsF <- sum(testOut$coordinates_transposed == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("jbd_Ctrans_chunker results TRUE", {
  testthat::expect_equal(resultsT, 22)
})
testthat::test_that("jbd_Ctrans_chunker results FALSE", {
  testthat::expect_equal(resultsF, 3)
})


# Test expected number of rows
testthat::test_that("jbd_Ctrans_chunker row count expected", {
  testthat::expect_equal(nrow(testOut), 25)
})
# Test output class
testthat::test_that("jbd_Ctrans_chunker expected class", {
  testthat::expect_type(testOut, "list")
})

