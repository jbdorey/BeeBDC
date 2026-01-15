

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
#testthat::test_that("DownloadTest_skipOnline",{
#skip_on_cran()
#  skip_on_ci()
  
userAgent <- paste0(sprintf("BeeBDC-tests/%s (R/%s.%s)",
                            as.character(utils::packageVersion("BeeBDC")),
                            R.version$major, R.version$minor),
                    "; R (",R.version$major,".", R.version$minor, " ",
                    R.version$platform, " ", R.version$arch," ",
                    R.version$os,")") 

print(userAgent)

FigToken <- "c04e8a7188995cdd9d3c04759d2874f621dc06b216b8c9af355a65e3e8d1748dfcafbf051e18cfe70f8c1e0534240c46ce988a72f93127137d6b2a30e1aacec6"

options(timeout=400,
        HTTPUserAgent =  userAgent)
Sys.setenv("FIGSHARE_TOKEN"=FigToken)


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
                                       headers = NULL)

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

#}) # END testthat::test_that
