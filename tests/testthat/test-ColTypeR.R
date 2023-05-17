requireNamespace("testthat")
testthat::test_that("ColTypeR class is correct", {
  testthat::expect_type(BeeDC::ColTypeR(), "list")
})
