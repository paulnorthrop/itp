#' Internal itp functions
#'
#' Internal itp functions
#' @details
#' These functions are not intended to be called by the user.
#' @name itp-internal
#' @keywords internal
NULL

# ============================ ITP loop using R ============================= #

#' @keywords internal
#' @rdname itp-internal
itp_r <- function(f, ..., a, b, ya, yb, epsilon, k1, k2, for_rk, inc) {
  k <- 0
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
    if (yITP * inc > 0) {
      b <- xITP
      yb <- yITP
    } else if (yITP * inc < 0) {
      a <- xITP
      ya <- yITP
    } else {
      a <- xITP
      ya <- yITP
      b <- xITP
      yb <- yITP
    }
    root <- (a + b) / 2
    # Update the first term of rk
    for_rk <- for_rk * 0.5
    k <- k + 1
  }
  val <- list(root = root, f.root = f(root, ...), iter = k, a = a,
              b = b, f.a = ya, f.b = yb, estim.prec = (b - a) / 2)
  return(val)
}
