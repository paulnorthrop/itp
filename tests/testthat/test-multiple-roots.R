# Check that for a function with multiple roots (Warsaw) we find 2 of them and
# (a) (b-a) / 2 is no larger than epsilon
# (b) f(a) and f(b) are of opposite signs

epsilon <- 1e-10

# Warsaw

warsaw <- function(x) ifelse(x > -1, sin(1 / (x + 1)), -1)
# Locally increasing
itp(warsaw, c(-1, 1), epsilon = epsilon)
res <- itp(staircase, c(-1, 1), epsilon = epsilon)
test_that("Warsaw increasing: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Warsaw increasing: f(a) and f(b) of opposite signs", {
  testthat::expect_false(sign(res$f.a) == sign(res$f.b))
})
# Locally decreasing
itp(warsaw, c(-1, 1), epsilon = epsilon)
res <- itp(staircase, c(-0.85, 0.8), epsilon = epsilon)
test_that("Warsaw decreasing: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Warsaw decreasing: f(a) and f(b) of opposite signs", {
  testthat::expect_false(sign(res$f.a) == sign(res$f.b))
})
