
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// User-supplied C++ functions for f.

// Note that currently the only interface available in itp is
// double fun(const double& x, const List& pars).

// Each function must be prefaced by the line: // [[Rcpp::export]]

// One-dimensional standard normal.

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

// A function to create external pointers to the functions to evaluate f.
// See http://gallery.rcpp.org/articles/passing-cpp-function-pointers/
// If you write a new function above called new_name then add something
// like the following.
//
// else if (fstr == "new_name")
//   return(XPtr<funcPtr>(new funcPtr(&new_name))) ;

//' Create external pointer to a C++ function for \code{f}
//'
//' @param fstr A string indicating the C++ function required.
//'
//' @export
// [[Rcpp::export]]
SEXP create_xptr(std::string fstr) {
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
  else
    return(XPtr<funcPtr>(R_NilValue)) ;
}

// We could create the external pointers when this file is sourced using
// this embedded R code below and/or (re)create them using the relevant
// pointer-creation functions in an R session or R package.

/*** R
ptr_wiki <- create_xptr("wiki")
ptr_lambert <- create_xptr("lambert")
ptr_trig1 <- create_xptr("trig1")
ptr_poly3 <- create_xptr("poly3")
*/

