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
# Create request
httr2::request(base_url = client$token_url) %>% 
  httr2::req_headers(BeeBDC_header = paste0("curl -H 'Authorization: token ", httr2::obfuscated(client$secret),
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
