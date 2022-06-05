#' itp: the Interpolate, Truncate, Project (ITP) Root-Finding Algorithm
#'
#' Implements the Interpolate, Truncate, Project (ITP) root-finding
#' algorithm developed by Oliveira and Takahashi (2021).
#' The user provides a the function, from the real numbers to the real
#' numbers, and an interval with the property that the values of the function
#' at its endpoints have different signs. If the function is continuous over
#' this interval then the ITP method estimates the value at which the function
#' is equal to zero. If the function is discontinuous then a point of
#' discontinuity at which the function changes sign may be found. Tuning
#' parameters of the ITP algorithm can be set by the user. Sensible default
#' values are set based on arguments in Oliveira and Takahashi (2021).
#'
#' @details The main function is \code{\link{itp}}.
#' See the vignette \code{vignette("itp-vignette", package = "itp")} for an
#' overview of the package.
#' @references Oliveira, I. F. D. and Takahashi, R. H. C. (2021). An Enhancement
#'   of the Bisection Method Average Performance Preserving Minmax Optimality,
#'   \emph{ACM Transactions on Mathematical Software}, \strong{47}(1), 1-24.
#'   \doi{10.1145/3423597}
#' @seealso \code{\link{itp}} for the ITP root-finding algorithm
#' @seealso \code{\link{itpPrint}} to print objects of class \code{"itp"}
#'   returned from \code{\link{itp}}.
#' @docType package
#' @name itp-package
NULL
