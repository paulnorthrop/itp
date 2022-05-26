#' Internal itp functions
#'
#' Internal itp functions
#' @details
#' These functions are not intended to be called by the user.
#' @name itp-internal
#' @keywords internal
NULL

# ======== The example used in the Wikipedia entry for the ITP method ======= #
#' @keywords internal
#' @rdname itp-internal
wiki <- function(x) x ^ 3 - x - 2

# ===== Example functions from Table 1 of Oliveira and Takahashi (2021) ===== #

### Well-behaved functions

# Lambert
#' @keywords internal
#' @rdname itp-internal
lambert <- function(x) x * exp(x) - 1

# Trigonometric 1
#' @keywords internal
#' @rdname itp-internal
trig1 <- function(x) tan(x - 1 /10)

# Trigonometric 2
#' @keywords internal
#' @rdname itp-internal
trig2 <- function(x) sin(x) + 1 / 2

# Polynomial 1
#' @keywords internal
#' @rdname itp-internal
poly1 <- function(x) 4 * x ^ 5 + x ^ 2 + 1

# Polynomial 2
#' @keywords internal
#' @rdname itp-internal
poly2 <- function(x) x + x ^ 10 - 1

# Exponential
#' @keywords internal
#' @rdname itp-internal
exponential <- function(x) (pi ^ x - exp(1))

# Logarithmic
#' @keywords internal
#' @rdname itp-internal
logarithmic <- function(x) log(abs(x - 10 / 9))

# Posynomial
#' @keywords internal
#' @rdname itp-internal
posynomial <- function(x) 1 / 3 + sign(x) * abs(x) ^ (1 / 3) + x ^ 3

# Weierstrass
#' @keywords internal
#' @rdname itp-internal
weierstrass <- function(x) 1/10^3 + sum(sin(pi*(1:10)^3*x/2)/(pi*(1:10)^3))

# Poly Frac
#' @keywords internal
#' @rdname itp-internal
polyfrac <- function(x) (x + 2 / 3) / (x + 101 / 100)

# Normal CDF
#' @keywords internal
#' @rdname itp-internal
normalcdf <- function(x) pnorm(x - 1) - sqrt(2) / 4

# Normal PDF
#' @keywords internal
#' @rdname itp-internal
normalpdf <- function(x) dnorm(x - 1) - sqrt(2) / 4

### Ill-behaved functions

## Non-simple zero

# Polynomial 3
#' @keywords internal
#' @rdname itp-internal
poly3 <- function(x) (x * 1e6 - 1) ^ 3

# Exp.Poly
#' @keywords internal
#' @rdname itp-internal
exp_poly <- function(x) exp(x) * (x * 10 ^ 6 - 1) ^ 3

# Tan Poly
#' @keywords internal
#' @rdname itp-internal
tan_poly <- function(x) (x - 1 / 3) ^ 2 * atan(x - 1 / 3)

# Circles
#' @keywords internal
#' @rdname itp-internal
circles <- function(x) sign(3*x+1) * (1 - sqrt(1 - (3*x+1)^2 / 9^2))

## Discontinuous

# Step function
#' @keywords internal
#' @rdname itp-internal
stepfn <- function(x) (x > 1e-6 - 1) * (1e-6 + 1) - 1

# Geometric
#' @keywords internal
#' @rdname itp-internal
geometric <- function(x) ifelse(abs(x-1/21) <= 1e-10, 0, 1 / (21 * x - 1))

# Trunc Poly
#' @keywords internal
#' @rdname itp-internal
truncpoly <- function(x) (x / 2) ^ 2 + ceiling(x / 2) - 1 / 2

# Staircase
#' @keywords internal
#' @rdname itp-internal
staircase <- function(x) ceiling(10 * x - 1) + 1 / 2

## Multiple roots

# Noisy line
#' @keywords internal
#' @rdname itp-internal
noisy_line <- function(x) x + sin(x * 10 ^ 6) / 10 + 1 / 10 ^ 3

# Warsaw
#' @keywords internal
#' @rdname itp-internal
warsaw <- function(x) ifelse(x > -1, sin(1 / (x + 1)), -1)

# Sawtooth
#' @keywords internal
#' @rdname itp-internal
sawtooth <- function(x) 202 * x - 2 * floor((2 * x + 1e-2) / 2e-2) - 1 / 10

# Sawtooth Cube
#' @keywords internal
#' @rdname itp-internal
sawtooth3 <- function(x) sawtooth(x) ^ 3
