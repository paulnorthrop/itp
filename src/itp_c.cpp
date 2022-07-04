
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' The Interpolate, Truncate, Project (ITP) root-finding algorithm
//'
//' Performs one-dimensional root-finding using the ITP algorithm of
//' Oliveira and Takahashi (2021). This function is equivalent to
//' \code{\link{itp}} but calculations are performed entirely using C++, and
//' the arguments differ slightly: \code{itp_c} has a named required argument
//' \code{pars} rather than \code{...} and it does not have the arguments
//' \code{interval}, \code{f.a} or \code{f.b}.
//' @param f An R function or an external pointer to a C++ function.  For the
//'   latter see the article
//'   \href{https://gallery.rcpp.org/articles/passing-cpp-function-pointers/}{
//'   Passing user-supplied C++ functions} in the
//'   \href{https://gallery.rcpp.org/}{Rcpp Gallery}. The function for which the
//'   root is sought.
//' @param f An external pointer to a C++ function that evaluates the function
//'   \eqn{f}.
//' @param pars A list of additional arguments to the function.  This may be an
//'   empty list.
//' @param a,b Numeric scalars. Lower (\code{a}) and upper \code{b} limits of
//'   the interval to be searched for a root.
//' @param epsilon A positive numeric scalar. The desired accuracy of the root.
//'   The algorithm continues until the width of the bracketing interval for the
//'   root is less than or equal to \code{2 * epsilon}.
//' @param k1,k2,n0 Numeric scalars. The values of the tuning parameters
//'   \ifelse{html}{\eqn{\kappa}\out{<sub>1</sub>}}{\eqn{\kappa_1}},
//'   \ifelse{html}{\eqn{\kappa}\out{<sub>2</sub>}}{\eqn{\kappa_2}},
//'   \ifelse{html}{\eqn{n}\out{<sub>0</sub>}}{\eqn{n_0}}.
//'   See the \strong{Details} section of \code{\link{itp}}.
//'
//'   The default value for \code{k1} in \code{\link{itp_c}} is set as
//'   the inadmissible value of \code{-1}
//'   (in reality \ifelse{html}{\eqn{\kappa}\out{<sub>1</sub>}}{\eqn{\kappa_1}}
//'   must be positive) as a device to set the same default value for \code{k1}
//'   as \code{\link{itp}}, that is, \code{k1 = 0.2 / (b - a)}.  If the input
//'   value of \code{k1} is less than or equal to 0 then, inside
//'   \code{\link{itp_c}}, \code{k1 = 0.2 / (b - a)} is set.
//' @details For details see \code{\link{itp}}.
//' @return An object (a list) of class \code{"itp"} with the same structure
//'   as detailed in the \strong{Value} section of \code{\link{itp}}, except
//'   that the attribute \code{f_name} is empty (equal to \code{""}).
//' @references Oliveira, I. F. D. and Takahashi, R. H. C. (2021). An Enhancement
//'   of the Bisection Method Average Performance Preserving Minmax Optimality,
//'   \emph{ACM Transactions on Mathematical Software}, \strong{47}(1), 1-24.
//'   \doi{10.1145/3423597}
//' @seealso \code{\link{print.itp}} and \code{\link{plot.itp}} for print and
//'   plot methods for objects of class \code{"itp"} returned from \code{itp_c}
//'   or \code{itp}.
//' @examples
//' wiki_ptr <- xptr_create("wiki")
//' wres <- itp_c(f = wiki_ptr, pars = list(), a = 1, b = 2, epsilon = 0.0005)
//' wres
//' plot(wres, main = "Wiki")
//' @export
// [[Rcpp::export]]
List itp_c(const SEXP& f, const List& pars, double& a, double& b,
           const double& epsilon = 1e-10, const double& k1 = -1.0,
           const double& k2 = 2.0, const double& n0 = 1.0) {
  // Check that a < b
  if (a >= b) {
    stop("a must be less than b") ;
  }
  // If k1 <=0 then the default value of k1, based on a and b
  double k1set ;
  if (k1 <= 0.0) {
    k1set = 0.2 / (b - a) ;
  } else {
    k1set = k1 ;
  }
  if (k2 < 1.0 || k2 >= 1.0 + (1.0 + sqrt(5.0)) / 2.0) {
    stop("k2 must be in [ 1, 1 + (1 + sqrt(5)) / 2 )") ;
  }
  if (n0 < 0.0) {
    stop("n0 must be non-negative") ;
  }
  // Save the input values of a and b for use in plot.itp
  double input_a = a ;
  double input_b = b ;
  // Unwrap pointer to untransformed target log-density.
  typedef double (*funcPtr)(const double& x, const List& pars) ;
  XPtr<funcPtr> xpfun(f) ;
  funcPtr fun = *xpfun ;
  // Check whether the root lies on a limit of the input interval
  double ya = fun(a, pars) ;
  double yb = fun(b, pars) ;
  NumericVector yvec = NumericVector::create(ya, yb) ;
  // Check that f(a) and f(b) are finite
  if (any(is_infinite(yvec) | is_na(yvec))) {
    stop("f(a) and f(b) must be finite") ;
  }
  double root, froot, estimprec ;
  // Check whether (a, b) already satisfies the convergence criterion
  if (b - a <= 2 * epsilon) {
    root = (a + b) * 0.5 ;
    froot = fun(root, pars) ;
    estimprec = (b - a) * 0.5 ;
    List val =  List::create(Named("root") = root, Named("f.root") = froot,
                             Named("iter") = 0, Named("a") = a, Named("b") = b,
                             Named("f.a") = ya, Named("f.b") = yb,
                             Named("estim.prec") = estimprec) ;
    val.attr("class") = "itp" ;
    return val ;
  }
  // Check whether the root lies on a limit of the input interval
  if (ya == 0.0) {
    List val =  List::create(Named("root") = a, Named("f.root") = 0.0,
                             Named("iter") = 0, Named("a") = a, Named("b") = b,
                             Named("f.a") = ya, Named("f.b") = yb,
                             Named("estim.prec") = NA_REAL) ;
    val.attr("class") = "itp" ;
    return val ;
  }
  if (yb == 0.0) {
    List val = List::create(Named("root") = b, Named("f.root") = 0.0,
                            Named("iter") = 0, Named("a") = a, Named("b") = b,
                            Named("f.a") = ya, Named("f.b") = yb,
                            Named("estim.prec") = NA_REAL) ;
    val.attr("class") = "itp" ;
    return val ;
  }
  // Check that f(a) and f(b) have opposite signs
  double signya = (ya > 0.0) - (ya < 0.0) ;
  double signyb = (yb > 0.0) - (yb < 0.0) ;
  if (signya * signyb > 0.0) {
    stop("f() values at end points not of opposite sign") ;
  }
  // Set for_rk
  // M_LOG2E is 1/log_e(2). log_2(x) = log_e(x) / log_e(2) = log_e(x) * M_LOG2E
  double log2e = log(epsilon) * M_LOG2E ;
  double log2bma = log(b - a) * M_LOG2E ;
  double for_rk = pow(2.0, n0 - 1 + log2e + std::ceil(log2bma - log2e)) ;
  // Initialise the counter k and implement the loop
  int k = 0 ;
  double xf, x12, delta, rk, xt, xITP, yITP ;
  while (b - a > 2.0 * epsilon) {
    // Interpolation. Regular falsi, equation (5)
    xf = (yb * a - ya * b) / (yb - ya) ;
    // Truncation
    x12 = (a + b) * 0.5 ;
    // Equation (13)
    int sigma = (x12 > xf) - (x12 < xf) ;
    delta = k1set * pow(b - a, k2) ;
    // Equation (14)
    if (delta <= fabs(x12 - xf)) {
      xt = xf + sigma * delta ;
    } else {
      xt = x12 ;
    }
    // Projection, equation (15)
    rk = for_rk - (b - a) * 0.5 ;
    if (fabs(xt - x12) <= rk) {
      xITP = xt ;
    } else {
      xITP = x12 - sigma * rk ;
    }
    // Update (a, b)
    yITP = fun(xITP, pars) ;
    if (yITP * signyb > 0.0) {
      b = xITP ;
      yb = yITP ;
    } else if (yITP * signyb < 0.0) {
      a = xITP ;
      ya = yITP ;
    } else {
      a = xITP ;
      ya = yITP ;
      b = xITP ;
      yb = yITP ;
    }
    root = (a + b) * 0.5 ;
    // Update the first term of rk
    for_rk *= 0.5 ;
    k += 1 ;
  }
  froot = fun(root, pars) ;
  estimprec = (b - a) * 0.5 ;
  List val = List::create(Named("root") = root, Named("f.root") = froot,
                          Named("iter") = k, Named("a") = a, Named("b") = b,
                          Named("f.a") = ya, Named("f.b") = yb,
                          Named("estim.prec") = estimprec) ;
  val.attr("class") = "itp" ;
  val.attr("f") = f ;
  val.attr("f_args") = pars ;
  val.attr("input_a") = input_a ;
  val.attr("input_b") = input_b ;
  val.attr("f_name") = "" ;
  val.attr("used_c") = true ;
  return val ;
}
