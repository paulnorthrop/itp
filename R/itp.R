# maxiter unnecessary because convergence is guaranteed.
#
#    Document the thing re epsilon not being too small
# 0. DESCRIPTION (add more, inc interval getting smaller and cont vs discont)
#    itp-package.R
#    README: maths, read in png files
# 1. Do stuff like uniroot()
#    Check for root on boundary and return estim.prec = NA
# 6. Tests
#    Correct roots, correct number of iterations, correct sign of f.root
# 7. A package help page. itp-package?
# 8. Check package
# 9. Check that it works with rugs
# 10. Submit to CRAN

#' The Interpolate, Truncate, Project (ITP) root-finding algorithm
#'
#' Performs one-dimensional root-finding using the ITP algorithm of
#' Oliveira and Takahashi (2021).  The function \code{itp} searches the
#' interval from \code{a} to \code{b} for a root (i.e. a zero) of the
#' function \code{f} with respect to its first argument. Each iteration
#' results in a bracketing interval for the root that is narrower than the
#' previous interval.
#'
#' @param f The function for which the root is sought.
#' @param interval A numeric vector \code{c(a, b)} of length 2
#'   containing the end points of the interval to be searched for the root.
#'   The function values at the end points must be of opposite signs.
#' @param ... additional named or unnamed arguments to be pass to \code{f}.
#' @param a,b An alternative way to set the lower and upper end points of the
#'   interval to be searched. The function values at these end points must be
#'   of opposite signs.
#' @param epsilon A positive numeric scalar. The desired accuracy of the root.
#'   The algorithm continues until the width of the bracketing interval for the
#'   root is less than or equal to \code{2 * epsilon}.
#' @param k1,k2,n0 the values of the tuning parameters
#'   \ifelse{html}{\eqn{\kappa}\out{<sub>1</sub>}}{\eqn{\kappa_1}},
#'   \ifelse{html}{\eqn{\kappa}\out{<sub>2</sub>}}{\eqn{\kappa_2}},
#'   \ifelse{html}{\eqn{n}\out{<sub>0</sub>}}{n_0}.
#'   See \strong{Details}.
#' @details Page 8 of Oliveira and Takahashi (2021) describes the ITP
#'   algorithm.  The Wikipedia entry for the
#'   \href{https://en.wikipedia.org/wiki/ITP_method}{ITP method} provides
#'   a summary.  If the input function \code{f} is continuous over the interval
#'   [\code{a}, \code{b}] then the value of \code{f} evaluated at the estimated
#'   root is (approximately) equal to 0. If \code{f} is discontinuous over the
#'   interval [\code{a}, \code{b}] then the bracketing interval returned after
#'   convergence has the property that the signs of the function \code{f} at
#'   the end points of this interval are different and therefore the estimated
#'   root may be a point of discontinuity at which the sign of \code{f}
#'   changes.
#'
#'   The ITP method requires at most
#'   \ifelse{html}{\eqn{n}\out{<sub>max</sub>} = \eqn{n}\out{<sub>1/2</sub>} +
#'     \eqn{n}\out{<sub>0</sub>}}{\eqn{n_{\rm max} = n_{1/2} + n_0}} iterations,
#'   where \ifelse{html}{\eqn{n}\out{<sub>1/2</sub>}}{\eqn{n_{1/2}}} is the
#'   smallest integer not less than
#'   \ifelse{html}{log\out{<sub>2</sub>}(b-a) / 2\eqn{\epsilon}}{
#'   \eqn{\log_2(b-a) / 2 \epsilon}}.
#'   If \ifelse{html}{\eqn{n}\out{<sub>0</sub>} = 0}{\eqn{n_0 = 0}} then the
#'   ITP method will require no more iterations than the bisection method.
#'   Depending on the function \code{f}, setting a larger value for
#'   \ifelse{html}{\eqn{n}\out{<sub>0</sub>}}{\eqn{n_0 = 0}}, e.g. the default
#'   setting \ifelse{html}{\eqn{n}\out{<sub>0</sub>}=1}{\eqn{n_0 = 1}} used by
#'   the \code{itp} function, may result in a smaller number of iterations.
#'
#'   The default values of the other tuning parameters
#'   (\code{epsilon = 1e-10, k1 = 0.1, k2 = 2 / (b - a)}) are set based on
#'   the numerical experiments presented in Section 3 of Oliveira and Takahashi
#'   (2021).
#' @return An object (a list) of class \code{"itp"} containing the following
#'   components:
#'   \item{root}{the location of the root, calculated as \code{(a+b)/2}, where
#'     [\code{a, b}] is the bracketing interval after convergence.}
#'   \item{f.root}{the value of the function evaluated at root.}
#'   \item{iter}{the number of iterations performed.}
#'   \item{a,b}{the end points of the bracketing interval [\code{a, b}] after
#'     convergence.}
#'   \item{f.a,f.b}{the values of function at \code{a} and \code{b} after
#'     convergence.}
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
#' itp(wiki, c(1, 2), epsilon = 0.0005, k1 = 0.1, n0 = 1)
#' # The default setting (with k1 = 0.2) wins by 1 iteration
#' itp(wiki, c(1, 2), epsilon = 0.0005, n0 = 1)
#'
#' #### ----- Some examples from Table 1 of Oliveira and Takahashi (2021)
#'
#' ### Well-behaved functions
#'
#' # Lambert
#' lambert <- function(x) x * exp(x) - 1
#' itp(lambert, c(-1, 1))
#'
#' # Trigonometric 1
#' trig1 <- function(x) tan(x - 1 /10)
#' itp(trig1, c(-1, 1))
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
itp <- function(f, interval, ..., a = min(interval), b = max(interval),
                epsilon = 1e-10, k1 = 0.2 / (b - a), k2 = 2, n0 = 1) {
  if (!missing(interval) && length(interval) != 2L) {
    stop("'interval' must be a vector of length 2")
  }
  if (!is.numeric(a) || !is.numeric(b) || a >= b) {
    stop("a < b  is not fulfilled")
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
  # Evaluate the function at the end points of the interval
  ya <- f(a, ...)
  yb <- f(b, ...)
  # Set n_1/2 in equation (3)
  n12 <- max(ceiling(log2((b - a) / epsilon) - 1), 0)
  nmax <- n12 + n0
  # Check that they have opposite signs
  if (!isTRUE(as.vector(sign(ya) * sign(yb) <= 0))) {
    stop("f() values at end points not of opposite sign")
  }
  k <- 0
#  continue <- FALSE
  for_rk <- epsilon * 2 ^ nmax
  while (b - a > 2 * epsilon) {
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
    root <- (a + b) / 2
    # Update the first term of rk
    for_rk <- for_rk * 0.5
    k <- k + 1
  }
  val <- list(root = root, f.root = f(root, ...), iter = k, a = a,
              b = b, f.a = ya, f.b = yb, estim.prec = (b - a) / 2)
  class(val) <- "itp"
  return(val)
}
