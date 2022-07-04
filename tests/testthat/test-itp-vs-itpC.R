# Check that results from itp() and itp_c() agree

wrap_itp_c <- function(f, interval, ..., epsilon = 1e-10,
                      k1 = 0.2 / (interval[2] - interval[1]), k2 = 2, n0 = 1) {
  itp_c(f = f, pars = list(...), a = interval[1], b = interval[2],
       epsilon = epsilon, k1 = k1, k2 = k2, n0 = n0)
}

## Wiki

wiki <- function(x) x ^ 3 - x - 2
res1 <- itp(wiki, c(1, 2), k1 = 0.1, n0 = 1)
wiki_ptr <- xptr_create("wiki")
res2 <- wrap_itp_c(wiki_ptr, c(1, 2), k1 = 0.1, n0 = 1)
test_that("Wiki: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

# Repeat for -wiki, to check for a locally decreasing function
neg_wiki <- function(x) -wiki(x)
res1 <- itp(neg_wiki, c(1, 2), k1 = 0.1, n0 = 1)
neg_wiki_ptr <- xptr_create("neg_wiki")
res2 <- wrap_itp_c(neg_wiki_ptr, c(1, 2), k1 = 0.1, n0 = 1)
test_that("Negated wiki: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

## Lambert

lambert <- function(x) x * exp(x) - 1
res1 <- itp(lambert, c(-1, 1))
lambert_ptr <- xptr_create("lambert")
res2 <- wrap_itp_c(lambert_ptr, c(-1, 1))
test_that("Lambert: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

## Trig 1

trig1 <- function(x, root) tan(x - root)
res1 <- itp(trig1, c(-1, 1), root = 1 / 10)
trig1_ptr <- xptr_create("trig1")
res2 <- wrap_itp_c(trig1_ptr, c(-1, 1), root = 1 / 10)
test_that("Trig 1: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

## Poly 3

poly3 <- function(x) (x * 1e6 - 1) ^ 3
res1 <- itp(poly3, c(-1, 1))
poly3_ptr <- xptr_create("poly3")
res2 <- wrap_itp_c(poly3_ptr, c(-1, 1))
test_that("Poly 3: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

# Linear

linear <- function(x) x
res1 <- itp(linear, c(-1, 1))
linear_ptr <- xptr_create("linear")
res2 <- wrap_itp_c(linear_ptr, c(-1, 1))
test_that("Linear: R vs C++", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})

# Staircase

staircase <- function(x) ceiling(10 * x - 1) + 1 / 2
res1 <- itp(staircase, c(-1, 1))
staircase_ptr <- xptr_create("staircase")
res2 <- wrap_itp_c(staircase_ptr, c(-1, 1))
test_that("Staircase: R vs C++ in (-1,1)", {
  testthat::expect_equal(res1, res2, ignore_attr = TRUE)
})
