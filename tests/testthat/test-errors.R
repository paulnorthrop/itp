# Check that errors are triggered

# Lambert
lambert <- function(x) x * exp(x) - 1

# End point error
test_that("Lambert: end point error", {
  testthat::expect_error(itp(lambert, c(-1, 0)))
})

# Missing interval
test_that("Lambert: missing interval", {
  testthat::expect_error(itp(lambert))
})

# Interval the wrong length
test_that("Lambert: interval length 1", {
  testthat::expect_error(itp(lambert, -1))
})
test_that("Lambert: interval length 3", {
  testthat::expect_error(itp(lambert, c(-1, 0, 1)))
})

# a >= b
test_that("Lambert: a >= b", {
  testthat::expect_error(itp(lambert, a = 1, b = -1))
})

# k1 <= 0
test_that("Lambert: k1 <= 0", {
  testthat::expect_error(itp(lambert, c(-1, 1), k1 = -1))
})

# Inappropriate k2, not in [ 1, 1 + (1 + sqrt(5)) / 2 )
test_that("Lambert: k2 = 0.99", {
  testthat::expect_error(itp(lambert, c(-1, 1), k2 = 0.99))
})
test_that("Lambert: k2 = upper limit + 0.01", {
  testthat::expect_error(itp(lambert, c(-1, 1),
                             k2 = 1 + (1 + sqrt(5)) / 2 + 0.01))
})

# n0 < 0
test_that("Lambert: n0 < 0", {
  testthat::expect_error(itp(lambert, c(-1, 1), n0 = -1))
})

