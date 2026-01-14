

# load in the test dataset
system.file("extdata", "testChecklist.rda", package="BeeBDC") |>
  load()


# Test the expected results
testthat::test_that("testChecklist expected number of columns", {
  testthat::expect_equal(ncol(testChecklist), 23)
})

# Test classes
testthat::test_that("testChecklist expected class", {
  testthat::expect_type(testChecklist, "list")
})
testthat::test_that("testChecklist expected class", {
  testthat::expect_equal(attributes(testChecklist)$class, c("spec_tbl_df", "tbl_df","tbl","data.frame" ))
})

# Set some options for accessing the data
options(timeout=400,
        HTTPUserAgent = "Test download for BeeBDC's R package")

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
  testthat::expect_equal(ncol(testChecklist), 23)
})

# Test classes
testthat::test_that("testChecklist expected class", {
  testthat::expect_type(testChecklist, "list")
})
testthat::test_that("testChecklist expected class", {
  testthat::expect_equal(attributes(testChecklist)$class, c("spec_tbl_df", "tbl_df","tbl","data.frame" ))
})

} #END !is.null
