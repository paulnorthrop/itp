# 0. How come uniroot() does so well, at least in terms of the number of
#    iterations?  Benchmark this comparison in a vignette?
#    Better than microbenchmark (See Radford Neal)
#    Put in the vignette?
#
#    Even if ITP is poorer than uniroot then I can still put itp on CRAN.
#
#    Why do Poly 1 and 2 not give the correct number of iterations?
#    Are there typos in the paper? I need to check my code very carefully!
#
# https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
#
# Produce a summary of itpvs uniroot for all the functions in Table 1?
# Put all the functions in itp-internal.R
#
# Problems with ...
#
# Polynomial 1, Polynomial 2, logarithmic, posynomial (34 not 32), plot frac,
#
# n0 = 1 often performs better. Make this the default and explain in Details
# and the vignette.  Note that my code doesn't employ any strategies to
# alleviate the problems cause by floating point arithmetic and direct to
# the stuff about r in the Oliveira and Takhashi (2021) paper.
#
# Is is true that the sign of f alternates? No: see the trig2 example.
#
# maxiter unnecessary because convergence is guaranteed.
#
#    tol -> epilson?
#    (lower, upper) -> (a, b)?
#    Document the thing re epsilon not being too small
#
#    Calculate r in an even more clever way?
#
# 1. Do stuff like uniroot()
#    Check for root on boundary and return estim.prec = NA
# 2. Tidy the code.  Can I make the fsign stuff faster?
# 3. Document the code.  Give a link to the argument re n0.
# 4. Examples plus vignette.  Put all the examples in the paper in the vignette
# 5. Summary method for "itp" objects.
# 6. Tests
#    Correct roots, correct number of iterations, correct sign of f.root
# 7. A package help page. itp-package?
# 8. Check package
# 9. Check that it works with rugs
# 10. Submit to CRAN

#' The Interpolate, Truncate, Project (ITP) root finding algorithm
#'
#' Performs one-dimensional root finding using the ITP algorithm of
#' Oliveira and Takahashi (2021).  The function \code{itp} searches the
#' interval from \code{lower} to \code{upper} for a root (i.e. a zero) of the
#' function \code{f} with respect to its first argument. Each iteration
#' results in a bracketing interval for the root that is narrower than the
#' previous interval.
#'
#' @param f The function for which the root is sought.
#' @param interval A numeric vector \code{c(lower, upper)} of length 2
#'   containing the end-points of the interval to be searched for the root.
#' @param ... additional named or unnamed arguments to be pass to \code{f}.
#' @param lower,upper lower and upper
#' @param tol A positive numeric scalar. The desired accuracy of the root.
#'   The algorithm continues until the width of the bracketing interval for the
#'   root is less than or equal to \code{2 * tol}.
#' @param k1,k2,n0 the values of tuning parameters \eqn{\kappa_1},
#'   \eqn{\kappa_2} and \eqn{n_0}.  See \strong{Details}.
#' @param fsign A character scalar that may be used to control the sign of the
#'   function \code{f} at the estimated root, that is, the sign of
#'   \code{f.root = f(root)} in the returned object. If \code{fsign = "ge"}
#'   then, if necessary, the algorithm continues beyond convergence based on
#'   \code{tol} until \code{f.root} is greater than or equal to 0. If
#'   \code{fsign = "le"} then \code{f.root} will be less than or equal to 0.
#'   If \code{fsign = "either"} (the default) then the sign of \code{f.root} is
#'   not constrained.
#' @details Page 8 of Oliveira and Takahashi (2021) describes the ITP
#'   algorithm.  The Wikipedia entry for the
#'   \href{https://en.wikipedia.org/wiki/ITP_method}{ITP method} provides
#'   a summary.
#'
#'   The ITP method requires at most \eqn{n_{max} = n_{1/2} + n_0} iterations,
#'   where \eqn{n_{1/2}} is the smallest integer not less than
#'   \eqn{\log_2 (b-a) / 2 \epsilon}.  If \eqn{n_0 = 0}, which is the default
#'   setting in \code{itp}, then the ITP method will require no more iterations
#'   than the bisection method. Depending on the function \code{f} setting a
#'   larger value for \eqn{n_0}, e.g. the default setting \eqn{n_0 = 1} used
#'   by the \code{itp} function, may result in a smaller number of iterations.
#'
#'   The default values of the other tuning parameters
#'   (\code{tol = 1e-10, k1 = 0.1, k2 = 2 / (upper - lower)}) are set based on
#'   the numerical experiments presented in Section 3 of Oliveira and Takahashi
#'   (2021).
#' @return An object (a list) of class \code{"itp"} containing the following
#'   components:
#'   \item{root}{the location of the root, calculated as \code{(a+b)/2}, where
#'     (\code{a, b}) is the bracketing interval after convergence.}
#'   \item{f.root}{the value of the function evaluated at root.}
#'   \item{iter}{the number of iterations performed.}
#'   \item{a,b}{the values in the bracketing interval (\code{a, b}).}
#'   \item{estim.prec}{an approximate estimated precision for \code{root},
#'     equal to the half the width of the final bracket for the root.}
#' @references Oliveira, I. F. D. and Takahashi, R. H. C. (2021). An Enhancement
#'   of the Bisection Method Average Performance Preserving Minmax Optimality,
#'   \emph{ACM Transactions on Mathematical Software}, \strong{47}(1), 1-24.
#'   \doi{10.1145/3423597}
#' @examples
#' #### ----- The example used in the Wikipedia entry for the ITP method
#'
#' wiki <- function(x) x ^ 3 - x - 2
#' itp(wiki, c(1, 2), tol = 0.0005, k1 = 0.1, n0 = 1)
#' # The default setting (with k1 = 0.2) wins by 1 iteration
#' itp(wiki, c(1, 2), tol = 0.0005, n0 = 1)
#'
#' #### ----- Some examples from Table 1 of Oliveira and Takahashi (2021)
#'
#' ### Well-behaved functions
#'
#' # Lambert
#' lambert <- function(x) x * exp(x) - 1
#' itp(lambert, c(-1, 1))
#' # Forcing f(root) to be <= 0 requires 1 more iteration
#' itp(lambert, c(-1, 1), fsign = "le")
#'
#' # Trigonometric 1
#' trig1 <- function(x) tan(x - 1 /10)
#' itp(trig1, c(-1, 1))
#' # Forcing f(root) to be >= 0 requires 1 more iteration
#' itp(trig1, c(-1, 1), fsign = "ge")
#'
#' ### Ill-behaved functions
#'
#' ## Non-simple zero
#'
#' # Polynomial 3
#' poly3 <- function(x) (x * 1e6 - 1) ^ 3
#' itp(poly3, c(-1, 1))
#' # Using n0 = 0 leads to fewer iterations, in this example
#' poly3 <- function(x) (x * 1e6 - 1) ^ 3
#' itp(poly3, c(-1, 1), n0 = 0)
#'
#' ## Discontinuous
#'
#' # Staircase
#' staircase <- function(x) ceiling(10 * x - 1) + 1 / 2
#' itp(staircase, c(-1, 1))
#'
#' ## Multiple roots
#'
#' # Warsaw
#' warsaw <- function(x) ifelse(x > -1, sin(1 / (x + 1)), -1)
#' itp(warsaw, c(-1, 1))
#' @export
itp <- function(f, interval, ..., lower = min(interval), upper = max(interval),
                tol = 1e-10, k1 = 0.2 / (upper - lower), k2 = 2, n0 = 1,
                fsign = c("either", "ge", "le")) {
  fsign <- match.arg(fsign)
  if (!missing(interval) && length(interval) != 2L) {
    stop("'interval' must be a vector of length 2")
  }
  if (!is.numeric(lower) || !is.numeric(upper) || lower >= upper) {
    stop("lower < upper  is not fulfilled")
  }
  if (k1 <= 0) {
    stop("k1 must be positive")
  }
  if (k2 < 1 || k2 >= 1 + (1 + sqrt(5)) / 2) {
    stop("k2 must be in [ 1, 1 + (1 + sqrt(5)) / 2 )")
  }
  if (n0 < 0) {
    stop("n0 must be non-negative")
  }
  # a is lower, b is upper
  a <- lower
  b <- upper
  # Evaluate the function at the end points of the interval
  ya <- f(a, ...)
  yb <- f(b, ...)
  # Set n_1/2 in equation (3)
  n12 <- max(ceiling(log2((b - a) / tol) - 1), 0)
  nmax <- n12 + n0
  # Check that they have opposite signs
  if (!isTRUE(as.vector(sign(ya) * sign(yb) <= 0))) {
    stop("f() values at end points not of opposite sign")
  }
  k <- 0
  continue <- FALSE
  for_rk <- tol * 2 ^ nmax
  while (b - a > 2 * tol || !continue) {
    # Interpolation. Regular falsi, equation (5)
    xf <- (yb * a - ya * b) / (yb - ya)
    # Truncation
    x12 <- (a + b) / 2
    # Equation (13)
    sigma <- sign(x12 - xf)
    delta <- k1 * (b - a) ^ k2
    # Equation (14)
    if (delta <= abs(x12 - xf)) {
      xt <- xf + sigma * delta
    } else {
      xt <- x12
    }
    # Projection, equation (15)
    rk <- for_rk - (b - a) / 2
    if (abs(xt - x12) <= rk) {
      xITP <- xt
    } else {
      xITP <- x12 - sigma * rk
    }
    # Update (a, b)
    yITP <- f(xITP, ...)
    if (yITP > 0) {
      b <- xITP
      yb <- yITP
    } else if (yITP < 0) {
      a <- xITP
      ya <- yITP
    } else {
      a <- xITP
      b <- xITP
    }
    # If necessary, check whether f.root has the required sign
    root <- (a + b) / 2
    if (fsign == "ge") {
      f.root <- f(root, ...)
      continue <- f.root >= 0
    } else if (fsign == "le") {
      f.root <- f(root, ...)
      continue <- f.root <= 0
    } else {
      continue <- TRUE
    }
    # Update the first term of rk
    for_rk <- for_rk * 0.5
    k <- k + 1
  }
  val <- list(root = root, f.root = f(root, ...), iter = k, lower = a,
              upper = b, estim.prec = (b - a) / 2)
  class(val) <- "itp"
  return(val)
}
