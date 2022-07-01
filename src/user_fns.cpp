
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// User-supplied C++ functions for f.
// The only interface is double fun(const double& x, const List& pars).
// The second (List) argument must be included even if the function has no
// additional arguments.
// Each function must be prefaced by the line: // [[Rcpp::export]]

// [[Rcpp::export]]
double wiki_cpp(const double& x, const List& pars) {
  return pow(x, 3.0) - x - 2 ;
}

// [[Rcpp::export]]
double lambert_cpp(const double& x, const List& pars) {
  return x * exp(x) - 1.0 ;
}

// [[Rcpp::export]]
double trig1_cpp(const double& x, const List& pars) {
  double root = pars["root"] ;
  return tan(x - root) ;
}

// [[Rcpp::export]]
double poly3_cpp(const double& x, const List& pars) {
  return pow(x * 1e6 - 1.0, 3.0) ;
}

// [[Rcpp::export]]
double linear_cpp(const double& x, const List& pars) {
  return x ;
}

// [[Rcpp::export]]
double warsaw_cpp(const double& x, const List& pars) {
  if (x <= -1) {
    return -1 ;
  }
  return sin(1.0 / (1.0 + x)) ;
}

// [[Rcpp::export]]
double staircase_cpp(const double& x, const List& pars) {
  return ceil(10.0 * x - 1.0) + 0.5 ;
}

// A function to create external pointers to the functions to evaluate f.
// See http://gallery.rcpp.org/articles/passing-cpp-function-pointers/
// If you write a new function above called new_name then add something
// like the following.
//
// else if (fstr == "new_name")
//   return(XPtr<funcPtr>(new funcPtr(&new_name))) ;

//' Create an external pointer to a C++ function
//'
//' This function is used in \code{\link[itp]{itp-package}} to create
//' external pointers to the C++ functions used as examples to illustrate the
//' use of the function \code{\link{itp}}.  These pointers are passed as the
//' argument \code{f} to \code{\link{itp}}.  To create their own examples
//' the user will need to create their own C++ function a function that is
//' similar to \code{xptr_create}.
//'
//' @param fstr A string indicating the C++ function required.
//' @details See the vignette
//' \href{https://paulnorthrop.github.io/itp/articles/itp-vignette.html}{
//' Overview of the itp package} and the file
//' \href{https://raw.githubusercontent.com/paulnorthrop/itp/main/src/user_fns.cpp}{
//' user_fns.cpp} for information.
//'
//' The example C++ functions available in \code{itp} are: \code{"wiki"},
//' \code{"lambert"}, \code{"trig1"}, \code{"poly3"}, \code{"linear"},
//' \code{"warsaw"} and \code{staircase}.
//' @return The external pointer.
//' @seealso \code{\link{xptr_eval}} for calling a C++ function using an
//'   external pointer.
//' @examples
//' lambert_ptr <- xptr_create("lambert")
//' res <- itp(lambert_ptr, c(-1, 1))
//' @export
// [[Rcpp::export]]
SEXP xptr_create(std::string fstr) {
  typedef double (*funcPtr)(const double& x, const List& pars) ;
  if (fstr == "wiki")
    return(XPtr<funcPtr>(new funcPtr(&wiki_cpp))) ;
  else if (fstr == "lambert")
    return(XPtr<funcPtr>(new funcPtr(&lambert_cpp))) ;
  else if (fstr == "trig1")
    return(XPtr<funcPtr>(new funcPtr(&trig1_cpp))) ;
  else if (fstr == "poly3")
    return(XPtr<funcPtr>(new funcPtr(&poly3_cpp))) ;
  else if (fstr == "linear")
    return(XPtr<funcPtr>(new funcPtr(&linear_cpp))) ;
  else if (fstr == "warsaw")
    return(XPtr<funcPtr>(new funcPtr(&warsaw_cpp))) ;
  else if (fstr == "staircase")
    return(XPtr<funcPtr>(new funcPtr(&staircase_cpp))) ;
  else
    return(XPtr<funcPtr>(R_NilValue)) ;
}

// We could create the external pointers when this file is sourced using
// this embedded R code below and/or (re)create them using the relevant
// pointer-creation functions in an R session or R package.

/*** R
ptr_wiki <- xptr_create("wiki")
ptr_lambert <- xptr_create("lambert")
ptr_trig1 <- xptr_create("trig1")
ptr_poly3 <- xptr_create("poly3")
ptr_linear <- xptr_create("linear")
ptr_warsaw <- xptr_create("warsaw")
ptr_staircase <- xptr_create("staircase")
*/

