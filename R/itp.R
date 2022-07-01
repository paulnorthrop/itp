#' The Interpolate, Truncate, Project (ITP) root-finding algorithm
#'
#' Performs one-dimensional root-finding using the ITP algorithm of
#' Oliveira and Takahashi (2021).  The function \code{itp} searches an
#' interval [\eqn{a}, \eqn{b}] for a root (i.e. a zero) of the
#' function \code{f} with respect to its first argument. Each iteration
#' results in a bracketing interval for the root that is narrower than the
#' previous interval.  If the function is discontinuous then a point of
#' discontinuity at which the function changes sign may be found.
#'
#' @param f An R function or an external pointer to a C++ function. The
#'   function for which the root is sought.
#' @param interval A numeric vector \code{c(a, b)} of length 2
#'   containing the end points of the interval to be searched for the root.
#'   The function values at the end points must be of opposite signs.
#' @param ... Additional named or unnamed arguments to be passed to \code{f}.
#' @param a,b An alternative way to set the lower and upper end points of the
#'   interval to be searched. The function values at these end points must be
#'   of opposite signs.
#' @param f.a,f.b The values of \code{f(a)} and \code{f(b)}, respectively.
#' @param epsilon A positive numeric scalar. The desired accuracy of the root.
#'   The algorithm continues until the width of the bracketing interval for the
#'   root is less than or equal to \code{2 * epsilon}. The value of
#'   \code{epsilon} should be greater than
#'   \ifelse{html}{2\out{<sup>-63</sup>}\code{(b-a)}}{\eqn{2^{-63}}(\code{b-a})}
#'   to avoid integer overflow.
#' @param k1,k2,n0 the values of the tuning parameters
#'   \ifelse{html}{\eqn{\kappa}\out{<sub>1</sub>}}{\eqn{\kappa_1}},
#'   \ifelse{html}{\eqn{\kappa}\out{<sub>2</sub>}}{\eqn{\kappa_2}},
#'   \ifelse{html}{\eqn{n}\out{<sub>0</sub>}}{\eqn{n_0}}.
#'   See \strong{Details}.
#' @details Page 8 of Oliveira and Takahashi (2021) describes the ITP
#'   algorithm and the roles of the tuning parameters
#'   \ifelse{html}{\eqn{\kappa}\out{<sub>1</sub>}}{\eqn{\kappa_1}},
#'   \ifelse{html}{\eqn{\kappa}\out{<sub>2</sub>}}{\eqn{\kappa_2}} and
#'   \ifelse{html}{\eqn{n}\out{<sub>0</sub>}}{\eqn{n_0}}.  The algorithm is
#'   described using pseudocode. The Wikipedia entry for the
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
#'   smallest integer not less than \ifelse{html}{log\out{<sub>2</sub>}((b-a) /
#'   2\eqn{\epsilon})}{\eqn{\log_2 ((b-a) / 2 \epsilon)}}.
#'   If \ifelse{html}{\eqn{n}\out{<sub>0</sub>} = 0}{\eqn{n_0 = 0}} then the
#'   ITP method will require no more iterations than the bisection method.
#'   Depending on the function \code{f}, setting a larger value for
#'   \ifelse{html}{\eqn{n}\out{<sub>0</sub>}}{\eqn{n_0 = 0}}, e.g. the default
#'   setting \ifelse{html}{\eqn{n}\out{<sub>0</sub>}=1}{\eqn{n_0 = 1}} used by
#'   the \code{itp} function, may result in a smaller number of iterations.
#'
#'   The default values of the other tuning parameters
#'   (\code{epsilon = 1e-10, k1 = 0.1, k2 = 2 / (b - a)}) are set based on
#'   arguments made in Oliveira and Takahashi (2021).
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
#'   If the root occurs at one of the input endpoints \code{a} or \code{b} then
#'   \code{iter = 0} and \code{estim.prec = NA}.
#'
#'   The return object also has the attributes \code{f} (the input R function
#'   or pointer to a C++ function \code{f}), \code{f_args} (a list of
#'   additional arguments to \code{f} provided in \code{...}), \code{f_name}
#'   (a function name extracted from \code{as.character(substitute(f))} or the
#'   form of the R function if \code{f} was not named), \code{used_c} (a
#'   logical scalar indicating whether \code{f} is an R function or a pointer
#'   to a C++ function) and \code{input_a} and \code{input_b} (the input values
#'   of \code{a} and \code{b}).  These attributes are used in
#'   \code{\link{plot.itp}} to produce a plot of the function \code{f} over the
#'   interval \code{(input_a, input_b)}.
#' @references Oliveira, I. F. D. and Takahashi, R. H. C. (2021). An Enhancement
#'   of the Bisection Method Average Performance Preserving Minmax Optimality,
#'   \emph{ACM Transactions on Mathematical Software}, \strong{47}(1), 1-24.
#'   \doi{10.1145/3423597}
#' @seealso \code{\link{print.itp}} to print objects of class \code{"itp"}
#'   returned from \code{\link{itp}}.
#' @examples
#' #### ----- The example used in the Wikipedia entry for the ITP method
#'
#' wiki <- function(x) x ^ 3 - x - 2
#' itp(wiki, c(1, 2), epsilon = 0.0005, k1 = 0.1, n0 = 1)
#' # The default setting (with k1 = 0.2) wins by 1 iteration
#' x <- itp(wiki, c(1, 2), epsilon = 0.0005, n0 = 1)
#' plot(x)
#'
#' wiki <- create_xptr("wiki")
#' itp(f = wiki, c(1, 2), epsilon = 0.0005, k1 = 0.1)
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
#' trig1 <- function(x, root) tan(x - root)
#' itp(trig1, c(-1, 1), root = 1 / 10)
#' #
#' trig1 <- create_xptr("trig1")
#' itp(f = trig1, c(-1, 1), root = 1 / 10)
#'
#' # Logarithmic
#' logarithmic <- function(x, shift) log(abs(x - shift))
#' itp(logarithmic, c(-1, 1), shift = 10 /9)
#'
#' # Linear
#' linear <- function(x) x
#' # Solution in one iteration
#' itp(linear, c(-1, 1))
#' # Solution at an input endpoint
#' itp(linear, c(-1, 0))
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
                f.a = f(a, ...), f.b = f(b, ...), epsilon = 1e-10,
                k1 = 0.2 / (b - a), k2 = 2, n0 = 1) {
  if (is.function(f)) {
    using_c <- FALSE
  } else if (inherits(f, "externalptr")) {
    using_c <- TRUE
  } else {
    stop("''f'' must be an R function or a pointer to a C++ function")
  }
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
  # Save the input values of a and b for use in plot.itp
  input_a <- a
  input_b <- b
  # If we are using C++ then calculate f(a) and f(b)
  if (using_c) {
    f.a <- do.call(callViaXPtr, list(a, list(...), f))
    f.b <- do.call(callViaXPtr, list(b, list(...), f))
  }
  # Create a function name
  temp <- as.character(substitute(f))
  if (using_c) {
    f_name <- ifelse(length(temp) > 1, temp[2], temp)
  } else {
    f_name <- ifelse(length(temp) > 1, temp[3], temp)
  }
  # Check whether the root lies on a limit of the input interval
  if (f.a == 0) {
    val <- list(root = a, f.root = 0, iter = 0, a = a,
                b = b, f.a = f.a, f.b = f.b, estim.prec = NA)
    attributes(val) <- c(attributes(val), list(f = f, f_args = list(...),
                        input_a = input_a, input_b = input_b, f_name = f_name,
                        used_c = using_c))
    class(val) <- "itp"
    return(val)
  } else if (f.b == 0) {
    val <- list(root = b, f.root = 0, iter = 0, a = a,
                b = b, f.a = f.a, f.b = f.b, estim.prec = NA)
    attributes(val) <- c(attributes(val), list(f = f, f_args = list(...),
                         input_a = input_a, input_b = input_b, f_name = f_name,
                         used_c = using_c))
    class(val) <- "itp"
    return(val)
  }
  # Set n_1/2 in equation (3)
  n12 <- max(ceiling(log2((b - a) / epsilon) - 1), 0)
  nmax <- n12 + n0
  # Check that they have opposite signs
  if (isFALSE(sign(f.a) * sign(f.b) <= 0)) {
    stop("f() values at end points not of opposite sign")
  }
  for_rk <- epsilon * 2 ^ nmax
  # Call itp_r() or itp_cpp() as appropriate
  if (using_c) {
    for_itp_cpp <- list(f = f, pars = list(...), a = a, b = b, ya = f.a,
                        yb = f.b, epsilon = epsilon, k1 = k1, k2 = k2,
                        for_rk = for_rk, inc = sign(f.b))
    val <- do.call("itp_cpp", for_itp_cpp)
  } else {
    val <- itp_r(f = f, ..., a = a, b = b, ya = f.a, yb = f.b,
                 epsilon = epsilon, k1 = k1, k2 = k2, for_rk = for_rk,
                 inc = sign(f.b))
  }
  attributes(val) <- c(attributes(val), list(f = f, f_args = list(...),
                       input_a = input_a, input_b = input_b, f_name = f_name,
                       used_c = using_c))
  class(val) <- "itp"
  return(val)
}
