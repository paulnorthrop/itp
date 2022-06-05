# Check that for some continuous example function that
# (a) (b-a) / 2 is no larger than epsilon
# (b) f(root) is close to zero

# The example used in the Wikipedia entry for the ITP method
wiki <- function(x) x ^ 3 - x - 2
res <- itp(wiki, c(1, 2), k1 = 0.1, n0 = 1)
test_that("Wiki cubic: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Wiki cubic: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})

# Lambert

lambert <- function(x) x * exp(x) - 1
res <- itp(lambert, c(-1, 1))
test_that("Lambert: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Lambert: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})

# Polynomial 3
poly3 <- function(x) (x * 1e6 - 1) ^ 3
res <- itp(poly3, c(-1, 1))
test_that("Polynomial 3: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Polynomial 3: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})
