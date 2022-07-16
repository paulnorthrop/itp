# Check that for some continuous example functions that
#  (a) (b-a) / 2 is no larger than epsilon
#  (b) f(root) is close to zero

wrap_itp_c <- function(f, interval, ..., epsilon = 1e-10,
                      k1 = 0.2 / (interval[2] - interval[1]), k2 = 2, n0 = 1) {
  itp_c(f = f, pars = list(...), a = interval[1], b = interval[2],
       epsilon = epsilon, k1 = k1, k2 = k2, n0 = n0)
}

epsilon <- 1e-10

# The example used in the Wikipedia entry for the ITP method
wiki <- function(x) x ^ 3 - x - 2
res <- itp(wiki, c(1, 2), k1 = 0.1, n0 = 1, epsilon = epsilon)
test_that("Wiki cubic: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Wiki cubic: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})
# If (a,b) satisfy the convergence criterion then we stop immediately
res2 <- itp(wiki, c(res$a, res$b), k1 = 0.1, n0 = 1, epsilon = epsilon)
test_that("Wiki cubic: if (a,b) good enough then stop, iter", {
  testthat::expect_equal(res2$iter, 0)
})
test_that("Wiki cubic: if (a,b) good enough then stop, root", {
  testthat::expect_equal(res2$root, res$root)
})
# Repeat for itp_c()
# If (a,b) satisfy the convergence criterion then we stop immediately
wiki_ptr <- xptr_create("wiki")
res2 <- wrap_itp_c(wiki_ptr, c(res$a, res$b))
test_that("Wiki cubic: if (a,b) good enough then stop, iter, itp_c", {
  testthat::expect_equal(res2$iter, 0)
})
test_that("Wiki cubic: if (a,b) good enough then stop, root, itp_c", {
  testthat::expect_equal(res2$root, res$root)
})

# Repeat for -wiki, to check for a locally decreasing function
neg_wiki <- function(x) -wiki(x)
res <- itp(neg_wiki, c(1, 2), k1 = 0.1, n0 = 1, epsilon = epsilon)
test_that("-Wiki cubic: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("-Wiki cubic: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})

# Lambert

lambert <- function(x) x * exp(x) - 1
res <- itp(lambert, c(-1, 1), epsilon = epsilon)
test_that("Lambert: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Lambert: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})
# Repeat for -lambert
neg_lambert <- function(x) -lambert(x)
res <- itp(neg_lambert, c(-1, 1), epsilon = epsilon)
test_that("-Lambert: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("-Lambert: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})

# Polynomial 3

poly3 <- function(x) (x * 1e6 - 1) ^ 3
res <- itp(poly3, c(-1, 1), epsilon = epsilon)
test_that("Polynomial 3: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Polynomial 3: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})
# Repeat for -poly3
neg_poly3 <- function(x) -poly3(x)
res <- itp(neg_poly3, c(-1, 1), epsilon = epsilon)
test_that("-Polynomial 3: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("-Polynomial 3: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})

# Trigonmetric 1

root <- 1 / 10
trig1 <- function(x, root) tan(x - root)
res <- itp(trig1, c(-1, 1), root = root, epsilon = epsilon)
test_that("Trigonometric 1: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Trigonometric 1: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})
test_that("Trigonometric 1: root correct", {
  testthat::expect_equal(res$root, root)
})

# Logarithmic

shift <- 10 / 9
logarithmic <- function(x, shift) log(abs(x - shift))
res <- itp(logarithmic, c(-1, 1), shift = shift, epsilon = epsilon)
test_that("Logarithmic: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Logarithmic: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})
test_that("Logarithmic: root correct", {
  testthat::expect_equal(res$root, shift - 1)
})
# Look for the root at 2.1
res <- itp(logarithmic, c(1, 3), shift = shift, epsilon = epsilon)
test_that("Logarithmic: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Logarithmic: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})
test_that("Logarithmic: root correct", {
  testthat::expect_equal(res$root, shift + 1)
})

# Linear

linear <- function(x) x
res <- itp(linear, c(-1, 1), epsilon = epsilon)
test_that("Linear: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("Linear: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})
# Repeat for -linear
neg_linear <- function(x) -linear(x)
res <- itp(neg_linear, c(-1, 1), epsilon = epsilon)
test_that("-Linear: tolerance", {
  testthat::expect_lte(res$b - res$a , 2 * epsilon)
})
test_that("-Linear: f approx 0", {
  testthat::expect_equal(res$f.root, 0)
})

# Linear, solution at an input endpoint

res <- itp(linear, c(-1, 0))
test_that("Linear: solution at a", {
  testthat::expect_equal(c(res$iter, res$root, res$f.root), c(0, 0, 0))
})
res <- itp(function(x) x, c(0, 1))
test_that("Linear: solution at b", {
  testthat::expect_equal(c(res$iter, res$root, res$f.root), c(0, 0, 0))
})
# Repeat for itp_c()
linear_ptr <- xptr_create("linear")
res <- wrap_itp_c(linear_ptr, c(-1, 0))
test_that("Linear: solution at a, itp_c", {
  testthat::expect_equal(c(res$iter, res$root, res$f.root), c(0, 0, 0))
})
res <- wrap_itp_c(linear_ptr, c(0, 1))
test_that("Linear: solution at b, itp_c", {
  testthat::expect_equal(c(res$iter, res$root, res$f.root), c(0, 0, 0))
})
