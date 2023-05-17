
requireNamespace("readr")
requireNamespace("tibble")


testData <- tibble::tribble(
              ~catalog_number, ~pollinator_family, ~pollinator_genus, ~pollinator_species, ~collection_method, ~collector_number, ~day_collected, ~month_collected, ~year_collected,                   ~location_description, ~location_name,    ~habitat, ~latitude,  ~longitude, ~basis_of_record,
                  "SFU7052435916",       "Halictidae",    "Secret",             "sp. 1",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
                  "SFU705917",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
                  "SFU7052346919",       "Halictidae",    "Secret",       "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
                  "SFU7052645920",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen",
                  "SFU705645921",       "Andrenidae",         "Secret",     "sp.",    "Pantrap, blue",               20L,            30L,            "Apr",           2007L, "Bear Hill Regional Park, Victoria CRD",           "BH", "Garry Oak", 48.545605, -123.406769,       "specimen"
              )



# Save a temporary version of these data
readr::write_csv(testData, paste0(tempdir(), "/testData.csv"))

testOut1 <- BeeDC::readr_EPEL(path = paste0(tempdir()),
                             inFile = "/testData.csv",
                             outFile = "testDataOut.csv",
                             dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


# Get a count of TRUE and FALSE column name matches
resultsT <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == TRUE)
resultsF <- sum(colnames(testOut1) %in% (BeeDC::ColTypeR()[[1]] %>% names()) == FALSE)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("readr_EPEL results columns TRUE", {
  testthat::expect_equal(resultsT, 21)
})
testthat::test_that("readr_EPEL results columns FALSE", {
  testthat::expect_equal(resultsF, 0)
})

testthat::test_that("readr_EPEL expected class", {
  testthat::expect_type(testOut1, "list")
})




