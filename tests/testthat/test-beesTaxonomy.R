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

# Return the user client
client <- FigShare_client()

# Define download headers
headers <- c("/v2/token HTTP/1.1",
             "api.figshare.com",
             "24c8a7dacb07c3cc2a865d6885f015cc1af6eec04804116189d68652b51a3b8d676fbd4f46658703be8e74a92cad7aae4404e93a560d6192919870da63afee3b",
             "24c8a7dacb07c3cc2a865d6885f015cc1af6eec04804116189d68652b51a3b8d676fbd4f46658703be8e74a92cad7aae4404e93a560d6192919870da63afee3b",
             "425112ba97b7f583e0405535eb3a942f24910e73") %>%
  stats::setNames(c("GET", "Host: ", "Authorization: token", "client_secret", "client_id"))
# Set some options for accessing the data
options(timeout=400,
        HTTPUserAgent = headers)

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
