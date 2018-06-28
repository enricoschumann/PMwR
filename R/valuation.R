## -*- truncate-lines: t; -*-
## Copyright (C) 2008-18  Enrico Schumann

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
                              cashflow = function(x, ...) x$amount * x$price,
                              instrument = function(x, ...) "cash",
                              flip.sign = TRUE,
                              ...) {

    if (!is.null(names(multiplier)))
        multiplier <- multiplier[x$instrument]

    istr <- if (is.null(instrument))
                NA
            else if (is.character(instrument))
                instrument
            else if (is.function(instrument))
                instrument(x, ...)
            else
                as.character(instrument)

    ans <- x
    ans$instrument <- character(length(x))
    ans$instrument[] <- istr
    ans$amount <- cashflow(x) * multiplier
    if (flip.sign)
        ans$amount <- ans$amount * -1    
    ans$price[] <- 1
    ans
}

## TODO: add to internal documentation?
jcf <- function(x, multiplier = 1,                          
                cashflow   = function(x) x$amount * x$price,
                instrument = function(x) "cash",
                flip.sign  = TRUE, ...) {
    if (!inherits(x, "journal"))
        warning("expected journal")
    if (!is.null(names(multiplier)))
        multiplier <- multiplier[x$instrument]
    if (is.character(instrument))
        istr  <- instrument
    else if (is.function(instrument))
        istr <- instrument(x)
    else
        istr <- as.character(instrument)

    ans <- x
    ans$instrument <- character(length(x))
    ans$instrument[] <- istr
    ans$amount <- cashflow(x) * multiplier
    if (flip.sign)
        ans$amount <- ans$amount * -1    
    ans$price[] <- 1
    ans
}

valuation.position <- function(x, price.table, multiplier = 1,
                               do.sum = FALSE, price.unit,
                               verbose = TRUE, ...) {

    instrument <- attr(x, "instrument")
    
    if (!is.null(names(multiplier)))
        multiplier <- multiplier[instrument]

    pos <- x != 0
    if (any(is.na(price.table[pos]))) {
        miss <- which(is.na(price.table) & pos, TRUE)
        warning("missing prices")
        if (verbose)
            print(data.frame(timestamp = .timestamp(x)[miss[,1]],
                             instrument = instrument(x)[miss[,2]]))
    }
    price.table[!pos] <- 0
    ans <- as.matrix(x) * price.table
    if (any(multiplier != 1))
        ans <- ans %*% diag(multiplier, length(instrument))

    colnames(ans) <- instrument
    if (do.sum)
        ans <- rowSums(ans)
    ans <- as.matrix(ans)
    attr(ans, "position") <- x
    ans
}

## TODO: add to internal documentation
pv <- function(x, multiplier = 1, price.table, price.unit,
               do.sum = FALSE, ...) {

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
