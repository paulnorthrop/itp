#' Print method for objects of class \code{"itp"}
#'
#' Prints objects of class \code{"itp"} returned from \code{\link{itp}}.
#' @param x An object inheriting from class \code{"itp"}, a result of a
#'   call to \code{\link{itp}}.
#' @param all A logical scalar.  If \code{all = FALSE} then only the estimated
#'   root, the value of the function at the root an the number of iterations
#'   are printed.  If \code{all = TRUE} then, in addition, the final bracketing
#'   interval (\code{lower, upper}) and the estimation precision are printed.
#' @param ... Further arguments to be passed to or from other methods. They are
#'   ignored in this function..
#' @return The argument \code{x} is returned, invisibly.
#' @seealso \code{\link{itp}} for the Interpolate, Truncate, Project (ITP) root
#'   finding algorithm.
#' @name itpMethods
NULL
## NULL

# ================================ print.itp ================================ #

#' Print method for objects of class \code{"summary.itp"}
#'
#' @rdname itpMethods
#' @export
print.itp <- function(x, all = FALSE,
                      digits = max(3L, getOption("digits") - 3L), ...) {
  if (!inherits(x, "itp")) {
    stop("use only with \"itp\" objects")
  }
  temp<- x
  names(temp) <- c("root", "f(root)", "iterations", "lower", "upper",
                   "precision")
  if (!all) {
    print.default(format(temp[1:3], digits = digits), print.gap = 2L,
                  quote = FALSE)
  } else {
    print.default(format(temp[1:6], digits = digits), print.gap = 2L,
                  quote = FALSE)
  }
  invisible(x)
}
