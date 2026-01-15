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

# Set some options for accessing the data
  # Return the user client
client <- FigShare_client()
options(timeout=400,
        HTTPUserAgent = paste0("Test download for BeeBDC's R package",
                               "curl -H 'Authorization: token ", httr2::obfuscated(client$secret),
                               "'  https://api.figshare.com/v2 client_id ", client$id
                               ))

# Define download headers
headers <- c("/v2/token HTTP/1.1",
             "api.figshare.com",
             "24c8a7dacb07c3cc2a865d6885f015cc1af6eec04804116189d68652b51a3b8d676fbd4f46658703be8e74a92cad7aae4404e93a560d6192919870da63afee3b",
             "24c8a7dacb07c3cc2a865d6885f015cc1af6eec04804116189d68652b51a3b8d676fbd4f46658703be8e74a92cad7aae4404e93a560d6192919870da63afee3b",
             "425112ba97b7f583e0405535eb3a942f24910e73") %>%
  stats::setNames(c("GET", "Host: ", "Authorization: token", "client_secret", "client_id"))



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
