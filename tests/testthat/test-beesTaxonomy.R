library(httr2)  
# load in the test dataset
system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |>
  load()


# Test the expected results
testthat::test_that("testTaxonomy expected number of columns", {
  testthat::expect_equal(ncol(testTaxonomy), 24)
})
testthat::test_that("testTaxonomy expect that number of unique ids == number of overall rows", {
  testthat::expect_equal(length(unique(testTaxonomy$id)), nrow(testTaxonomy))
})
testthat::test_that("testTaxonomy expect that number of 0[accid] == accepted[taxostatus]", {
  testthat::expect_equal(sum(testTaxonomy$accid == 0), 
                         sum(testTaxonomy$taxonomic_status == "accepted"))
})

  # Test classes
testthat::test_that("testTaxonomy expected class", {
  testthat::expect_type(testTaxonomy, "list")
})
testthat::test_that("testTaxonomy expected class", {
  testthat::expect_equal(attributes(testTaxonomy)$class, c("spec_tbl_df", "tbl_df","tbl","data.frame" ))
})


# Define download headers
#headers <- c(Authorization = paste("token", "20271361303ce042ff9cce49ecd9c8f23594ed4032e83f150e535dbd2b67297ea88448e3ca29260f6c416f581834094a898089d90f2229111845f01ef8b75f46"))
## Set some options for accessing the data
#options(timeout=400,
#        HTTPUserAgent = paste0(names(headers), " ", headers, collapse = " ", sep = " "))
  # Set a well-formed, single-string UA just for this scope  

withr::local_options(list(
  HTTPUserAgent = sprintf("BeeBDC-tests/%s (R/%s.%s)",
                          as.character(utils::packageVersion("BeeBDC")),
                          R.version$major, R.version$minor)  ))
token <- "20271361303ce042ff9cce49ecd9c8f23594ed4032e83f150e535dbd2b67297ea88448e3ca29260f6c416f581834094a898089d90f2229111845f01ef8b75f46"
headers <- c("User-Agent" = sprintf("BeeBDC-tests/%s (R/%s.%s)",
                                    as.character(utils::packageVersion("BeeBDC")),
                                    R.version$major, R.version$minor),
             "Authorization" = sprintf("token %s", token))

  # TEST the full data
OS <- dplyr::if_else(.Platform$OS.type == "unix",
                     "MacLinux",
                     "Windows")
if(OS == "Windows"){
  mode <- "wb"
}else{
  mode <- "wb"
}
taxonomyFile <- NULL
# load in the full dataset
taxonomyFile <- BeeBDC::beesTaxonomy(mode = mode,
                                     headers = headers)


if(!is.null(taxonomyFile)){
# Test the expected results
testthat::test_that("beesTaxonomy expected number of columns", {
  testthat::expect_equal(ncol(taxonomyFile), 24)
})
testthat::test_that("beesTaxonomy expect that number of unique ids == number of overall rows", {
  testthat::expect_equal(length(unique(taxonomyFile$id)), nrow(taxonomyFile))
})
testthat::test_that("beesTaxonomy expect that number of 0[accid] == accepted[taxostatus]", {
  testthat::expect_equal(sum(taxonomyFile$accid == 0), 
                         sum(taxonomyFile$taxonomic_status == "accepted"))
})

# Test classes
testthat::test_that("beesTaxonomy expected class", {
  testthat::expect_type(taxonomyFile, "list")
})
testthat::test_that("beesTaxonomy expected class", {
  testthat::expect_equal(attributes(taxonomyFile)$class, c("spec_tbl_df", "tbl_df","tbl","data.frame" ))
})

} #END !is.null
