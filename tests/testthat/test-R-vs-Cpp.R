# Check that results from R and C++ agree

## Wiki

wiki <- function(x) x ^ 3 - x - 2
res1 <- itp(wiki, c(1, 2), k1 = 0.1, n0 = 1)
wiki_ptr <- create_xptr("wiki")
res2 <- itp(wiki_ptr, c(1, 2), k1 = 0.1, n0 = 1)
test_that("Wiki: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

## Lambert

lambert <- function(x) x * exp(x) - 1
res1 <- itp(lambert, c(-1, 1))
lambert_ptr <- create_xptr("lambert")
res2 <- itp(lambert_ptr, c(-1, 1))
test_that("Lambert: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

## Trig 1

trig1 <- function(x, root) tan(x - root)
res1 <- itp(trig1, c(-1, 1), root = 1 / 10)
trig1_ptr <- create_xptr("trig1")
res2 <- itp(trig1_ptr, c(-1, 1), root = 1 / 10)
test_that("Trig 1: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

## Poly 3

poly3 <- function(x) (x * 1e6 - 1) ^ 3
res1 <- itp(poly3, c(-1, 1))
poly3_ptr <- create_xptr("poly3")
res2 <- itp(poly3_ptr, c(-1, 1))
test_that("Poly 3: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})
