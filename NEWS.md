# itp 1.2.1.9000

## Bug fixes and minor improvements

* The formatting of 'x star' in the README file has been changed so that it renders correctly on GitHub.

# itp 1.2.1

## Bug fixes and minor improvements

* The issue described at https://github.com/RcppCore/Rcpp/issues/1287 has been fixed to avoid WARNINGs from CRAN checks on some platforms. Thank you to Dirk Eddelbuettel for providing the fix so quickly!

# itp 1.2.0

## New features

* The function `itp_c`, which implements the ITP algorithm entirely in C++.

# itp 1.1.0

## New features

* The argument `f` to the function `itp()` can now be either an R function or an external pointer to a C++ function. In the latter case, `itp()` makes use of the [Passing user-supplied C++ functions](https://gallery.rcpp.org/articles/passing-cpp-function-pointers/) framework offered by the Rcpp package. For the simple examples given in the `itp` package only a modest improvement in speed is observed (and expected).  However, being able to call `itp()` on the C++ side may have benefits in more challenging problems.  
* The package vignette and README file include examples based on external pointers to a C++ function.
* A plot method is provided, which plots the function `f` provided to `itp` over the input interval `(a, b)`.

## Bug fixes and minor improvements

* Explicit examples of passing arguments to the input function `f` have been added.

# itp 1.0.1

## Bug fixes and minor improvements

* A bug in the part of `itp()` in which the bracketing interval has been fixed. It caused incorrect results in cases where the function is locally decreasing.
* Tests have been added to check the correction of this bug.
