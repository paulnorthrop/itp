#' Plot method for objects of class \code{"itp"}
#'
#' Plot method for objects of class \code{"itp"} returned from
#' \code{\link{itp}}.
#' @param x An object inheriting from class \code{"itp"}, a result of a
#'   call to \code{\link{itp}}.
#' @param ... Arguments passed to \code{\link[graphics]{curve}},
#'   such as graphical parameters.
#' @details Uses \code{\link[graphics]{curve}} to produce a plot of the
#'   function \code{f} provided to \code{\link{itp}} over the interval within
#'   which a root was sought. The estimated root is indicated using a
#'   horizontal line drawn at 0 and a vertical line drawn at the estimated
#'   root. By default the name of the function \code{f} is used as a title,
#'   but this can be replaced by supplying the argument \code{main}.  The
#'   interval over which \code{f} is plotted can be changed by supplying the
#'   arguments \code{from} and/or \code{to}.
#' @return No return value, only the plot is produced.
#' @examples
#' # Lambert
#' lambert <- function(x) x * exp(x) - 1
#' x <- itp(lambert, c(-1, 1))
#' plot(x)
#' @seealso \code{\link{itp}} for the Interpolate, Truncate, Project (ITP) root
#'   finding algorithm.
#' @export
plot.itp <- function(x, ...) {
  # Extract f and any additional arguments to f
  f <- attr(x, "f")
  f_args <- attr(x, "f_args")
  # If f is a C++ function
  if (attr(x, "used_c")) {
    plotfun <- function(x) {
      not_vectorised <- function(x) {
        do.call(callViaXPtr, list(x, f_args, f))
      }
      return(vapply(x, not_vectorised, 0.0))
    }
  } else {
    if (length(f_args) == 0) {
      plotfun <- f
    } else {
      plotfun <- function(x) {
        return(do.call(f, c(list(x = x), f_args)))
      }
    }
  }
  curvefn <- function(x, ..., from = attr(x, "input_a"),
                      to = attr(x, "input_b"), xlab = "x", ylab = "f(x)",
                      main = attr(x, "f_name"), lwd = 2) {
    graphics::curve(plotfun, ..., from = from, to = to, xlab = xlab,
                    ylab = ylab, main = main, lwd = lwd)
    graphics::abline(h = 0)
    graphics::abline(v = x$root)
  }
  curvefn(x, ...)
  return(invisible())
}
