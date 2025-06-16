requireNamespace("dplyr")
requireNamespace("stringr")

  # Read in a flagged test dataset
beesFlagged <- BeeBDC::beesFlagged

  # Create a dummy "priorData" dataset using the first fifty rows
priorRun <- beesFlagged %>%
  dplyr::slice_head(n = 50)


  #### 1.0 Exclude ASP ####
  # Run the function using the first fifty to be matched to their original database_id numbers
testOut <- BeeBDC::idMatchR(
  currentData = beesFlagged %>% dplyr::mutate(database_id = database_id %>%
                                                stringr::str_replace("[0-9]+","") %>%
                                                paste0(., dplyr::row_number())),
  priorData = priorRun,
    # default completeness_cols
  completeness_cols = c("decimalLatitude",  "decimalLongitude",
                        "scientificName", "eventDate"),
  # First matches will be given preference over later ones
  matchBy = dplyr::lst(c("gbifID"),
                        c("catalogNumber", "institutionCode", "dataSource"),
                        c("occurrenceID", "dataSource"),
                        c("recordId", "dataSource"),
                        c("id"),
                        # Because INHS was entered as it's own dataset but is now included in the GBIF download...
                        c("catalogNumber", "institutionCode")),
  # You can exclude datasets from prior by matching their prefixs — before first underscore:
  # Which datasets are static and should be excluded from matching?
  excludeDataset = c("ASP", "BMin", "BMont", "CAES", "EaCO", "Ecd", "EcoS",
                     "Gai", "KP", "EPEL", "CAES", "EaCO", "FSCA", "SMC", "Lic", "Arm"))


# Get a count of TRUE and FALSE column name matches
resultsMatched <- sum(testOut$database_id %in% beesFlagged$database_id)
resultsExcluded <- sum(testOut$database_id %in% (beesFlagged %>% dplyr::mutate(database_id = database_id %>%
                                                                                   stringr::str_replace("[0-9]+","") %>%
                                                                                   paste0(., dplyr::row_number())) %>%
                                                     dplyr::pull(database_id)))
resultsNotMatched <- sum(testOut$database_id %in% beesFlagged$database_id)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("idMatchR results successfuly matched", {
  testthat::expect_equal(resultsMatched, 50)
})
testthat::test_that("idMatchR results not matched", {
  testthat::expect_equal(resultsNotMatched, 50)
})
testthat::test_that("idMatchR results excluded because in excludeDatasets", {
  testthat::expect_equal(resultsExcluded, 1)
})

testthat::test_that("idMatchR expected class", {
  testthat::expect_type(testOut, "list")
})


  #### 2.0 Don't exclude ASP ####
# Run the function using the first fifty to be matched to their original database_id numbers
testOut2 <- BeeBDC::idMatchR(
  currentData = beesFlagged %>% dplyr::mutate(database_id = database_id %>%
                                                stringr::str_replace("[0-9]+","") %>%
                                                paste0(., dplyr::row_number())),
  priorData = priorRun,
  # First matches will be given preference over later ones
  matchBy = dplyr::lst(c("gbifID"),
                        c("catalogNumber", "institutionCode", "dataSource"),
                        c("occurrenceID", "dataSource"),
                        c("recordId", "dataSource"),
                        c("id"),
                        # Because INHS was entered as it's own dataset but is now included in the GBIF download...
                        c("catalogNumber", "institutionCode")),
  # You can exclude datasets from prior by matching their prefixs — before first underscore:
  # Which datasets are static and should be excluded from matching?
    # This time don't exclude the ASP data
  excludeDataset = NULL)

# Get a count of TRUE and FALSE column name matches
resultsMatched <- sum(testOut2$database_id %in% beesFlagged$database_id)
resultsExcluded <- sum(testOut2$database_id %in% (beesFlagged %>% dplyr::mutate(database_id = database_id %>%
                                                                                 stringr::str_replace("[0-9]+","") %>%
                                                                                 paste0(., dplyr::row_number())) %>%
                                                   dplyr::pull(database_id)))
resultsNotMatched <- sum(testOut2$database_id %in% beesFlagged$database_id)

# Test the number of expected TRUE and FALSE columns and then test the output format (data frames and
# tibbles are a special case of lists)
testthat::test_that("idMatchR results successfuly matched", {
  testthat::expect_equal(resultsMatched, 50)
})
testthat::test_that("idMatchR results not matched", {
  testthat::expect_equal(resultsNotMatched, 50)
})
testthat::test_that("idMatchR results excluded because in excludeDatasets", {
  testthat::expect_equal(resultsExcluded, 0)
})


