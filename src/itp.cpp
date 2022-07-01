
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
List itp_cpp(const SEXP& f, const List& pars, double& a, double& b,
             double& ya, double& yb, const double& epsilon, const double& k1,
             const double& k2, double& for_rk, double& inc) {
  // Unwrap pointer to untransformed target log-density.
  typedef double (*funcPtr)(const double& x, const List& pars) ;
  XPtr<funcPtr> xpfun(f) ;
  funcPtr fun = *xpfun ;
  // Initialise the counter k and implement the loop
  int k = 0 ;
  double xf, x12, delta, rk, xt, xITP, yITP, root, froot, estimprec ;
  while (b - a > 2 * epsilon) {
    // Interpolation. Regular falsi, equation (5)
    xf = (yb * a - ya * b) / (yb - ya) ;
    // Truncation
    x12 = (a + b) * 0.5 ;
    // Equation (13)
    int sigma = (x12 > xf) - (x12 < xf) ;
    delta = k1 * pow(b - a, k2) ;
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
    if (yITP * inc > 0.0) {
      b = xITP ;
      yb = yITP ;
    } else if (yITP * inc < 0.0) {
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
    for_rk = for_rk * 0.5 ;
    k = k + 1 ;
  }
  froot = fun(root, pars) ;
  estimprec = (b - a) * 0.5 ;
  return List::create(Named("root") = root, Named("f.root") = froot,
                      Named("iter") = k, Named("a") = a, Named("b") = b,
                      Named("f.a") = ya, Named("f.b") = yb,
                      Named("estim.prec") = estimprec) ;
}

//' Call a C++ function using an external pointer
//'
//' This function is used in \code{\link{plot.itp}} to plot a function and
//' the root estimated by \code{\link{itp}}.
//' @param x The main argument of the function.
//' @param pars A list of additional arguments to the function.  This may be an
//'   empty list.
//' @param xpsexp An external pointer to a C++ function.
//' @details See the
//' \href{https://gallery.rcpp.org/articles/passing-cpp-function-pointers/}{
//' Passing user-supplied C++ functions} article in the
//' \href{https://gallery.rcpp.org/}{Rcpp Gallery} for information.
//' @return A numeric scalar: the value of the C++ function evaluated at the
//'   input values \code{x} and \code{pars}.
//' @examples
//' lambert_ptr <- xptr_create("lambert")
//' res <- itp(lambert_ptr, c(-1, 1))
//'
//' # Value at lower limit
//' xptr_eval(-1, list(), lambert_ptr)
//'
//' # Value at upper limit
//' xptr_eval(1, list(), lambert_ptr)
//'
//' # Value at the estimated root
//' xptr_eval(res$root, list(), lambert_ptr)
//' @seealso \code{\link{xptr_create}} for creating an external pointer to a
//'   C++ function.
//' @export
// [[Rcpp::export]]
double xptr_eval(const double& x, const List&pars, SEXP xpsexp) {
  typedef double (*funcPtr)(const double& x, const List& pars) ;
  XPtr<funcPtr> xpfun(xpsexp);
  funcPtr fun = *xpfun;
  double y = fun(x, pars);
  return (y);
}
