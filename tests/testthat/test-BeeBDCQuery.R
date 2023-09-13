
requireNamespace("dplyr")
requireNamespace("lubridate")
requireNamespace("mgsub")

# Download the datasets
beesTaxonomy <- BeeBDC::beesTaxonomy()
beesChecklist <- BeeBDC::beesChecklist()


if(!is.null(beesTaxonomy) & !is.null(beesChecklist)){
  # Run example queries
# Single entry example
 testQuerySing <- BeeBDCQuery(
   beeName = "Homalictus fijiensis",
   searchChecklist = TRUE,
   printAllSynonyms = TRUE,
   beesTaxonomy = beesTaxonomy,
   beesChecklist = beesChecklist)
 
   # Multiple entry example
 testQueryMult <- BeeBDCQuery(
   beeName = c("Homalictus fijiensis", "Homalictus urbanus",
   "Lasioglossum fijiense (Perkins and Cheesman, 1928)"),
   searchChecklist = TRUE,
   printAllSynonyms = TRUE,
   beesTaxonomy = beesTaxonomy,
   beesChecklist = beesChecklist)
 
  # Simple without checklist
 testQuerySimple <- BeeBDCQuery(
   beeName = c("Homalictus fijiensis", "Homalictus urbanus",
               "Lasioglossum fijiense (Perkins and Cheesman, 1928)"),
   searchChecklist = FALSE,
   printAllSynonyms = TRUE,
   beesTaxonomy = beesTaxonomy,
   beesChecklist = beesChecklist)

 
 # Test class
 testthat::test_that("BeeBDCQuery expected class", {
   testthat::expect_type(testQuerySing, "list")
 })
 testthat::test_that("BeeBDCQuery expected class", {
   testthat::expect_type(testQueryMult, "list")
 })
  # Test that elements are tibbles
 testthat::test_that("BeeBDCQuery expected class", {
   testthat::expect_true(any(stringr::str_detect(attributes(testQuerySing$taxonomyReport)$class, 
                                                 "tbl_df")))
 })
 testthat::test_that("BeeBDCQuery expected class", {
   testthat::expect_true(any(stringr::str_detect(attributes(testQueryMult$taxonomyReport)$class, 
                                                 "tbl_df")))
 })
 
 # Test output lengths
 testthat::test_that("BeeBDCQuery list size", {
   testthat::expect_equal(length(testQuerySing), 4)
 })
 testthat::test_that("BeeBDCQuery list size", {
   testthat::expect_equal(length(testQueryMult), 4)
 })
 testthat::test_that("BeeBDCQuery list size", {
   testthat::expect_equal(length(testQuerySimple), 3)
 })
 
 }
