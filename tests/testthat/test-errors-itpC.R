# Check that errors are triggered in itpC()

wrap_itpC <- function(f, interval, ..., epsilon = 1e-10,
                      k1 = 0.2 / (interval[2] - interval[1]), k2 = 2, n0 = 1) {
  itpC(f = f, pars = list(...), a = interval[1], b = interval[2],
       epsilon = epsilon, k1 = k1, k2 = k2, n0 = n0)
}

# Log
log_ptr <- xptr_create("log")

# f not finite at both end points
test_that("f(a) or f(b) not finite", {
  testthat::expect_error(wrap_itpC(f = log_ptr, c(0, 1)))
})

# Lambert
lambert_ptr <- xptr_create("lambert")

# a >= b
test_that("Lambert: a >= b", {
  testthat::expect_error(wrap_itpC(lambert_ptr, c(1, -1)))
})

# End point error
test_that("Lambert: end point error", {
  testthat::expect_error(wrap_itpC(lambert_ptr, c(-1, 0)))
})

# Missing interval
test_that("Lambert: missing interval", {
  testthat::expect_error(wrap_itpC(lambert_ptr))
})

# Interval the wrong length
test_that("Lambert: interval length 1", {
  testthat::expect_error(wrap_itpC(lambert_ptr, -1))
})
test_that("Lambert: interval length 3", {
  testthat::expect_error(wrap_itpC(lambert_ptr, c(-1, 0, 1)))
})

# a >= b
test_that("Lambert: a >= b", {
  testthat::expect_error(wrap_itpC(lambert_ptr, a = 1, b = -1))
})

# k1 <= 0
test_that("Lambert: k1 <= 0", {
  testthat::expect_error(wrap_itpC(lambert_ptr, c(-1, 1), k1 = -1))
})

# Inappropriate k2, not in [ 1, 1 + (1 + sqrt(5)) / 2 )
test_that("Lambert: k2 = 0.99", {
  testthat::expect_error(wrap_itpC(lambert_ptr, c(-1, 1), k2 = 0.99))
})
test_that("Lambert: k2 = upper limit + 0.01", {
  testthat::expect_error(wrap_itpC(lambert_ptr, c(-1, 1),
                             k2 = 1 + (1 + sqrt(5)) / 2 + 0.01))
})

# n0 < 0
test_that("Lambert: n0 < 0", {
  testthat::expect_error(wrap_itpC(lambert_ptr, c(-1, 1), n0 = -1))
})

# f not an R function or an external pointer
test_that("Wrong f", {
  testthat::expect_error(wrap_itpC(f = "lambert_ptr", c(-1, 1)))
})
