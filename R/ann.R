## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-
## Time-stamp: <2015-01-27 18:13:22 CET (es)>

ann <- function(x, ...) {
    warning(sQuote("ann"), " is deprecated. ",
            "Use  returns(x, t, period = 'ann')  instead.")
    UseMethod("ann", x)
}
    
ann.default <- function(x, t, ...) {
    ann(zoo(x, t))
}

ann.zoo <- function(x, ...) {
    xi <- index(x)
    start <- xi[1L]
    end <- xi[lx <- length(xi)]
    t <- as.numeric(end - start)/365
    (coredata(x[lx])/coredata(x[1L]))^(1/t) - 1

}

ann.NAVseries <- function(x, ...) {
    ann(as.zoo.NAVseries(x))
}
