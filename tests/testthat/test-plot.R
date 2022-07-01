# Check that plot.itp works for
# (a) R function with no additional arguments
# (b) C++ pointer with no additional arguments
# (c) R function with additional arguments
# (d) C++ pointer with additional arguments

## Lambert

# (a)
lambert <- function(x) x * exp(x) - 1
res <- itp(lambert, c(-1, 1))
test_that("Lambert in R: plot.itp OK", {
  testthat::expect_silent(plot(res))
})

# (b)
lambert_ptr <- create_xptr("lambert")
res <- itp(lambert_ptr, c(-1, 1))
test_that("Lambert in C++: plot.itp OK", {
  testthat::expect_silent(plot(res))
})

## Trigonometric 1

# (c)
trig1 <- function(x, root) tan(x - root)
res <- itp(trig1, c(-1, 1), root = 1 / 10)
test_that("Trig 1 in R: plot.itp OK", {
  testthat::expect_silent(plot(res))
})

# (d)
trig1_ptr <- create_xptr("trig1")
itp(f = trig1_ptr, c(-1, 1), root = 1 / 10)
test_that("Lambert in C++: plot.itp OK", {
  testthat::expect_silent(plot(res))
})
