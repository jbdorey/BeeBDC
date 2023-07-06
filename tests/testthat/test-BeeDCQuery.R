
requireNamespace("dplyr")
requireNamespace("lubridate")
requireNamespace("mgsub")

  # Run example queries
# Single entry example
 testQuerySing <- BeeDCQuery(
   beeName = "Homalictus fijiensis",
   searchChecklist = TRUE,
   printAllSynonyms = TRUE)
 
   # Multiple entry example
 testQueryMult <- BeeDCQuery(
   beeName = c("Homalictus fijiensis", "Homalictus urbanus",
   "Lasioglossum fijiense (Perkins and Cheesman, 1928)"),
   searchChecklist = TRUE,
   printAllSynonyms = TRUE)
 
  # Simple without checklist
 testQuerySimple <- BeeDCQuery(
   beeName = c("Homalictus fijiensis", "Homalictus urbanus",
               "Lasioglossum fijiense (Perkins and Cheesman, 1928)"),
   searchChecklist = FALSE,
   printAllSynonyms = TRUE)

 
 # Test class
 testthat::test_that("BeeDCQuery expected class", {
   testthat::expect_type(testQuerySing, "list")
 })
 testthat::test_that("BeeDCQuery expected class", {
   testthat::expect_type(testQueryMult, "list")
 })
  # Test that elements are tibbles
 testthat::test_that("BeeDCQuery expected class", {
   testthat::expect_true(any(stringr::str_detect(attributes(testQuerySing$taxonomyReport)$class, 
                                                 "tbl_df")))
 })
 testthat::test_that("BeeDCQuery expected class", {
   testthat::expect_true(any(stringr::str_detect(attributes(testQueryMult$taxonomyReport)$class, 
                                                 "tbl_df")))
 })
 
 # Test output lengths
 testthat::test_that("BeeDCQuery list size", {
   testthat::expect_equal(length(testQuerySing), 4)
 })
 testthat::test_that("BeeDCQuery list size", {
   testthat::expect_equal(length(testQueryMult), 4)
 })
 testthat::test_that("BeeDCQuery list size", {
   testthat::expect_equal(length(testQuerySimple), 3)
 })
 
 
