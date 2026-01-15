

# load in the test dataset
system.file("extdata", "testChecklist.rda", package="BeeBDC") |>
  load()


# Test the expected results
testthat::test_that("testChecklist expected number of columns", {
  testthat::expect_equal(ncol(testChecklist), 24)
})

# Test classes
testthat::test_that("testChecklist expected class", {
  testthat::expect_type(testChecklist, "list")
})
testthat::test_that("testChecklist expected class", {
  testthat::expect_equal(attributes(testChecklist)$class, c("tbl_df","tbl","data.frame" ))
})

# Set some options for accessing the data
  # Return the user client
client <- FigShare_client()
options(timeout=400,
        HTTPUserAgent = paste0("Test download for BeeBDC's R package",
                               "curl -H 'Authorization: token ", httr2::obfuscated(client$secret),
                               "'  https://api.figshare.com/v2 client_id ", client$id
                               ))


# TEST the full data

OS <- dplyr::if_else(.Platform$OS.type == "unix",
                     "MacLinux",
                     "Windows")
if(OS == "Windows"){
  mode <- "wb"
}else{
  mode <- "wb"
}

beesChecklist <- NULL
# load in the full dataset
beesChecklist <- BeeBDC::beesChecklist(mode = mode)

if(!is.null(beesChecklist)){
# Test the expected results
testthat::test_that("testChecklist expected number of columns", {
  testthat::expect_equal(ncol(testChecklist), 24)
})

# Test classes
testthat::test_that("testChecklist expected class", {
  testthat::expect_type(testChecklist, "list")
})
testthat::test_that("testChecklist expected class", {
  testthat::expect_equal(attributes(testChecklist)$class, c("tbl_df","tbl","data.frame" ))
})

} #END !is.null
