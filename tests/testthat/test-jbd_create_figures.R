requireNamespace("dplyr")

beesFlagged <- BeeBDC::beesFlagged


#### 1.0 prefilter ####
figuresP <-
   BeeBDC::jbd_create_figures(
     data = dplyr::tibble(beesFlagged %>% dplyr::select(!.uncer_terms)),
     path = paste0(tempdir()),
     database_id = "database_id",
     workflow_step = "prefilter",
     save_figures = TRUE)


# Test the number of time figures
testthat::test_that("jbd_create_figures number of figures", {
  testthat::expect_equal(length(figuresP), 5)
})
   testthat::test_that("jbd_create_figures expected class", {
     testthat::expect_type(figuresP, "list")
   })
   testthat::test_that("jbd_create_figures expected class", {
     testthat::expect_type(figuresP[[1]], "object")
   })



#### 2.0 space ####
figuresS <-
  BeeBDC::jbd_create_figures(
    data = dplyr::tibble(beesFlagged %>% dplyr::select(!.uncer_terms)),
    path = paste0(tempdir()),
    database_id = "database_id",
    workflow_step = "space",
    save_figures = TRUE)

# Test the number of space figures
testthat::test_that("jbd_create_figures number of figures", {
  testthat::expect_equal(length(figuresS), 4)
})
   testthat::test_that("jbd_create_figures expected class", {
     testthat::expect_type(figuresS, "list")
   })
   testthat::test_that("jbd_create_figures expected class", {
     testthat::expect_type(figuresS[[1]], "object")
   })

#### 3.0 time ####
figuresT <-
    BeeBDC::jbd_create_figures(
      data = dplyr::tibble(beesFlagged %>% dplyr::select(!.uncer_terms)),
      path = paste0(tempdir()),
      database_id = "database_id",
      workflow_step = "time",
      save_figures = TRUE)


# Test the number of time figures
testthat::test_that("jbd_create_figures number of figures", {
  testthat::expect_equal(length(figuresT), 5)
})
 testthat::test_that("jbd_create_figures expected class", {
   testthat::expect_type(figuresT, "list")
 })
 testthat::test_that("jbd_create_figures expected class", {
   testthat::expect_type(figuresT[[1]], "object")
 })


