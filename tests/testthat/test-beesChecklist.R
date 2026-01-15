library(httr2)

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


# Define download headers
headers <- c(Authorization = paste("token", "425112ba97b7f583e0405535eb3a942f24910e73"))
# Set some options for accessing the data
options(timeout=400,
        HTTPUserAgent = paste0(names(headers), " ", headers, collapse = " ", sep = " "))



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
beesChecklist <- BeeBDC::beesChecklist(mode = mode,
                                       headers = headers)

if(!is.null(beesChecklist)){
# Test the expected results
testthat::test_that("beesChecklist expected number of columns", {
  testthat::expect_equal(ncol(beesChecklist), 22)
})

# Test classes
testthat::test_that("beesChecklist expected class", {
  testthat::expect_type(beesChecklist, "list")
})
testthat::test_that("beesChecklist expected class", {
  testthat::expect_equal(attributes(beesChecklist)$class, c("tbl_df","tbl","data.frame" ))
})

} #END !is.null
