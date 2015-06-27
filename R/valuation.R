## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

## valuation <- function(x, ...) {
##     UseMethod("valuation")
## }
## valuation.default <- function(x, ...) {
##     do.call(x, list(...))
## }
## valuation.instrument <- function(x, ...,
##                                  data = function(x, ...) list(...)) {

##     do.call(x$vfun, data(x, ...))
## }


valuation <- function(x, ...) {
    UseMethod("valuation")
}

valuation.default <- function(x, ...) {
    force(x)
}

valuation.journal <- function(x,
                              multiplier = 1,                          
                              cashflow = function(x) x$amount * x$price,
                              flip.sign = TRUE,
                              ...) {

    if (!is.null(names(multiplier)))
        multiplier <- multiplier[x$instrument]
    ans <- x
    ans$instrument[] <- "cash"
    ans$amount <- cashflow(x) * multiplier
    if (flip.sign)
        ans$amount <- ans$amount * -1    
    ans$price[] <- 1
    ans
}

## TODO: add to internal documentation
jcf <- function(x, multiplier = 1,                          
                cashflow = function(x) x$amount * x$price,
                flip.sign = TRUE, ...) {
    if (!is.null(names(multiplier)))
        multiplier <- multiplier[x$instrument]
    ans <- x
    ans$instrument[] <- "cash"
    ans$amount <- cashflow(x) * multiplier
    if (flip.sign)
        ans$amount <- ans$amount * -1    
    ans$price[] <- 1
    ans
}

valuation.position <- function(x, multiplier = 1, price.table,
                               do.sum = FALSE, unit, ...) {

    if (!is.null(names(multiplier)))
        multiplier <- multiplier[x$instrument]

    ans <- x$position * price.table
    if (any(multiplier != 1)) {
        ans <- ans %*% diag(multiplier, length(x$instrument))
        colnames(ans) <- x$instrument
    }
    if (do.sum)
        ans <- rowSums(ans)
    ans
}

## TODO: add to internal documentation
pv <- function(x, multiplier = 1, price.table,
                               do.sum = FALSE, unit, ...) {

    instrument <- attr(x, "instrument")
    
    if (!is.null(names(multiplier)))
        multiplier <- multiplier[instrument]

    ans <- x * price.table
    if (any(multiplier != 1))
        ans <- ans %*% diag(multiplier, length(instrument))

    colnames(ans) <- instrument
    if (do.sum)
        ans <- rowSums(ans)
    ans
}
