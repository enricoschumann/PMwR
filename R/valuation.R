## -*- truncate-lines: t; -*-
## Copyright (C) 2008-20  Enrico Schumann

valuation <- function(x, ...)
    UseMethod("valuation")


valuation.default <- function(x, ...) {
    force(x)
}
## valuation.default <- function(x, ...) {
##     do.call(x, list(...))
## }


## valuation.instrument <-
##     function(x, ...,
##              data = function(x, ...) list(...)) {
##         do.call(x$vfun, data(x, ...))
## }


valuation.journal <-
    function(x,
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

        cf <- if (is.null(cashflow))
                  NA
              else if (is.numeric(cashflow))
                  cashflow
              else if (is.function(cashflow))
                  cashflow(x, ...)

    ans <- x
    ans$instrument <- character(length(x))
    ans$instrument[] <- istr
    ans$amount <- cf * multiplier
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

valuation.position <- function(x,
                               vprice,
                               multiplier = 1,
                               do.sum = FALSE,
                               price.unit,
                               use.names = FALSE,
                               verbose = TRUE,
                               do.warn = TRUE, ...) {

    instrument <- attr(x, "instrument")

    if (!is.null(names(multiplier)))
        multiplier <- multiplier[instrument]

    if (do.warn && any(is.na(multiplier))) {
        warning("NAs in ", sQuote("multiplier"))
    }
    if (use.names &&
        !is.null(dim(vprice))) {
        vprice <- vprice[, instrument]
    }
    if (use.names &&
        nrow(x) == 1L &&
        is.null(dim(vprice)) &&
        !is.null(names(vprice)) &&
        !all(is.na(instrument))) {
        vprice_ <- rep(NA_real_, ncol(x))
        names(vprice_) <- instrument
        ii <- instrument %in% names(vprice)
        vprice_[ii] <- vprice[instrument[ii]]
        vprice <- t(vprice_)
    }

    if (!all(dim(vprice) == dim(x)))
        warning("dimensions of position and ", sQuote("vprice"), " differ")

    pos <- x != 0
    is.na.prices <- is.na(vprice)
    miss <- which(is.na.prices & pos, TRUE)
    if (do.warn && nrow(miss)) {
        warning("missing prices")
        if (verbose) {
            for (i in seq_len(ncol(x)))
                if (any(pos[, i]) && all(is.na.prices[, i]))
                    warning("no prices at all for ", instrument[i])
            print(data.frame(timestamp  = .timestamp(x)[miss[, 1L]],
                             instrument = instrument(x)[miss[, 2L]]))
        }
    }
    vprice[!pos] <- 0
    ans <- as.matrix(x) * vprice
    if (any(multiplier != 1))
        ans <- ans %*% diag(multiplier, length(instrument))

    colnames(ans) <- instrument
    if (do.sum)
        ans <- rowSums(ans)
    ans <- as.matrix(ans)
    attr(ans, "position") <- x
    ans
}

## TODO: use valuation instead
pv <- function(x, multiplier = 1, vprice, price.unit,
               do.sum = FALSE, ...) {

    instrument <- attr(x, "instrument")

    if (!is.null(names(multiplier)))
        multiplier <- multiplier[instrument]

    ans <- x * vprice
    if (any(multiplier != 1))
        ans <- ans %*% diag(multiplier, length(instrument))

    colnames(ans) <- instrument
    if (do.sum)
        ans <- rowSums(ans)
    ans
}
