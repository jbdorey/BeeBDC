requireNamespace("testthat")
testthat::test_that("ColTypeR class is correct", {
  testthat::expect_type(BeeBDC::ColTypeR(), "list")
})

testthat::test_that("ColTypeR class is correct", {
  testthat::expect_equal(attributes(BeeBDC::ColTypeR())$class, "col_spec")
})
