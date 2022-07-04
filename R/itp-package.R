#' itp: The Interpolate, Truncate, Project (ITP) Root-Finding Algorithm
#'
#' Implements the Interpolate, Truncate, Project (ITP) root-finding
#' algorithm developed by Oliveira and Takahashi (2021).
#' The user provides a function, from the real numbers to the real numbers, and
#' an interval with the property that the values of the function at its
#' endpoints have different signs. If the function is continuous over this
#' interval then the ITP method estimates the value at which the function is
#' equal to zero. If the function is discontinuous then a point of
#' discontinuity at which the function changes sign may be found. The function
#' can be supplied using either an R function or an external pointer to a C++
#' function. Tuning parameters of the ITP algorithm can be set by the user.
#' Default values are set based on arguments in Oliveira and Takahashi (2021).
#'
#' @details The main function is \code{\link{itp}}.
#' See the vignette
#' \href{https://paulnorthrop.github.io/itp/articles/itp-vignette.html}{
#' Overview of the itp package}, which can also be accessed using
#' \code{vignette("itp-vignette", package = "itp")}.
#' @references Oliveira, I. F. D. and Takahashi, R. H. C. (2021). An Enhancement
#'   of the Bisection Method Average Performance Preserving Minmax Optimality,
#'   \emph{ACM Transactions on Mathematical Software}, \strong{47}(1), 1-24.
#'   \doi{10.1145/3423597}
#' @seealso \code{\link{itp}} for the ITP root-finding algorithm
#' @seealso \code{\link{print.itp}} and \code{\link{plot.itp}} for print and
#'   plot methods for objects of class \code{"itp"} returned from \code{itp}.
#' @seealso \code{\link{xptr_create}} and \code{\link{xptr_eval}} for
#'   creating external pointers to the C++ functions used as examples in this
#'   package and evaluating those functions.
#' @docType package
#' @aliases itp-package
#' @useDynLib itp, .registration = TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"
