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

# Set some options for accessing the data
# Return the user client
client <- FigShare_client()
options(timeout=400,
        HTTPUserAgent = paste0("Test download for BeeBDC's R package",
                               "curl -H 'Authorization: token ", httr2::obfuscated(client$secret),
                               "'  https://api.figshare.com/v2 client_id ", client$id
        ))
  # Create request
httr2::request(base_url = client$token_url) %>% 
  httr2::req_headers(Host = "api.figshare.com",
                     Authorization = httr2::obfuscated(client$secret),
                     BeeBDC_header = paste0("curl -H 'Authorization: token ", httr2::obfuscated(client$secret),
                               "'  https://api.figshare.com/v2"),
                     client_id = client$id,
                     client_secret = httr2::obfuscated(client$secret),
                     name = client$name)
# Define download headers
headers <- c("/v2/token HTTP/1.1",
             "api.figshare.com",
             httr2::obfuscated(client$secret)) %>%
  stats::setNames(c("GET", "Host", "Authorization"))

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
