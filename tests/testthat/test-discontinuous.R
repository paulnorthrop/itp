# Check that for some continuous example functions that
# (a) (b-a) / 2 is no larger than epsilon
# (b) f(a) and f(b) are of opposite signs

epsilon <- 1e-10

# Staircase

staircase <- function(x) ceiling(10 * x - 1) + 1 / 2
res <- itp(staircase, c(-1, 1), epsilon = epsilon)
test_that("Staircase: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Staircase: f(a) and f(b) of opposite signs", {
  testthat::expect_false(sign(res$f.a) == sign(res$f.b))
})
# Repeat for -staircase
neg_staircase <- function(x) -staircase(x)
res <- itp(neg_staircase, c(-1, 1), epsilon = epsilon)
test_that("-Staircase: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("-Staircase: f(a) and f(b) of opposite signs", {
  testthat::expect_false(sign(res$f.a) == sign(res$f.b))
})

# Trunc poly

truncpoly <- function(x) (x / 2) ^ 2 + ceiling(x / 2) - 1 / 2
res <- itp(truncpoly, c(-1, 1), epsilon = epsilon)
test_that("Truncpoly: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Truncpoly: f(a) and f(b) of opposite signs", {
  testthat::expect_false(sign(res$f.a) == sign(res$f.b))
})
# Repeat for -truncpoly
neg_truncpoly <- function(x) -truncpoly(x)
res <- itp(neg_truncpoly, c(-1, 1), epsilon = epsilon)
test_that("-Truncpoly: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("-Truncpoly: f(a) and f(b) of opposite signs", {
  testthat::expect_false(sign(res$f.a) == sign(res$f.b))
})
