valuation <- function(x, ...) {
    UseMethod("valuation")
}
valuation.default <- function(x, ...) {
    do.call(x, list(...))
}
valuation.instrument <- function(x, ...,
                                 data = function(x, ...) list(...)) {

    do.call(x$vfun, data(x, ...))
}
