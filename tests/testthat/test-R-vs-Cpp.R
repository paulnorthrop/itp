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

# Linear

linear <- function(x) x
res1 <- itp(linear, c(-1, 1))
linear_ptr <- create_xptr("linear")
res2 <- itp(linear_ptr, c(-1, 1))
test_that("Linear: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

# Warsaw

warsaw <- function(x) ifelse(x > -1, sin(1 / (x + 1)), -1)
warsaw_ptr <- create_xptr("warsaw")

# Function increasing over the interval
res1 <- itp(warsaw, c(-1, 1))
res2 <- itp(warsaw_ptr, c(-1, 1))
test_that("Warsaw: R vs C++ in (-1,1)", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

# Function decreasing over the interval
res1 <- itp(warsaw, c(-0.85, -0.8))
res2 <- itp(warsaw_ptr, c(-0.85, -0.8))
test_that("Warsaw: R vs C++ in (-0.85,-0.8)", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

# Staircase

staircase <- function(x) ceiling(10 * x - 1) + 1 / 2
res1 <- itp(staircase, c(-1, 1))
staircase_ptr <- create_xptr("staircase")
res2 <- itp(staircase_ptr, c(-1, 1))
test_that("Staircase: R vs C++ in (-1,1)", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})
