

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
# headers <- c(Authorization = paste("token", "20271361303ce042ff9cce49ecd9c8f23594ed4032e83f150e535dbd2b67297ea88448e3ca29260f6c416f581834094a898089d90f2229111845f01ef8b75f46"))
# # Set some options for accessing the data
testthat::test_that("DownloadTest_skipOnline",{
  # Skip this test on MAC on GITHUB due to 403 error in tests
  if(stringr::str_detect(Sys.info() %>% as.character(), "Darwin") %>% any()){
    skip_on_ci()}
#  skip_on_ci()
Sys.getenv("BEEBDC_SECRET_GITHUB")
Authorization <- Sys.getenv("BEEBDC_SECRET_GITHUB")
userAgent <- paste0(sprintf("BeeBDC-tests/%s (R/%s.%s)",
                            as.character(utils::packageVersion("BeeBDC")),
                            R.version$major, R.version$minor),
                    "; R (",R.version$major,".", R.version$minor, " ",
                    R.version$platform, " ", R.version$arch," ",
                    R.version$os,")") 
options(timeout=400,
        HTTPUserAgent =  userAgent)


OS <- dplyr::if_else(.Platform$OS.type == "unix",
                     "MacLinux",
                     "Windows")
  # select mode — cna be varied by OS if needed
if(OS == "Windows"){
  mode <- "wb"
}else{
  mode <- "wb"
}

beesChecklist <- NULL
# load in the full dataset
beesChecklist <- BeeBDC::beesChecklist(mode = mode,
                                       headers = NULL,
                                       token = Authorization)

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

}) # END testthat::test_that
