# Check that the print function works

epsilon <- 1e-10

# The example used in the Wikipedia entry for the ITP method
wiki <- function(x) x ^ 3 - x - 2
res <- itp(wiki, c(1, 2), k1 = 0.1, n0 = 1, epsilon = epsilon)

res1 <- print(res, all = FALSE)
test_that("print, all = FALSe", {
  testthat::expect_equal(res, res1)
})
res2 <- print(res, all = TRUE)
test_that("print, all = FALSe", {
  testthat::expect_equal(res, res2)
})