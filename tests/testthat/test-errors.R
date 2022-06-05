# Check that errors are triggered

# Lambert, end point error

lambert <- function(x) x * exp(x) - 1
test_that("Lambert: end point error", {
  testthat::expect_error(itp(lambert, c(-1, 0), epsilon = epsilon))
})
