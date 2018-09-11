## -*- truncate-lines: t; -*-
## Copyright (C) 2008-18  Enrico Schumann

div_adjust <- function(x, t, div, backward = TRUE, additive = FALSE) {

    ## TODO  the function should work for
    ##       matrices of one column as well
    if (!is.null(dim(x)))
        stop(sQuote("x"), " must be a vector")
    tmp <- t > 1L & t <= length(x)
    if (all(!tmp))
        return(x)

    if (length(div) == 1L && length(t) > 1L)
        div <- rep(div, length(t))

    div <- div[tmp]
    t <- t[tmp]

    if (length(t) > 1L && length(div) == 1L)
        div <- rep(div, length(t))
    else if (length(div) != length(t))
        stop("different lengths for ",
             sQuote("div"), " and ", sQuote("t"))

    div <- div[tmp]
    t <- t[tmp]
    n <- length(x)
    if (!additive) {
        x_ <- x
        x_[t] <- x_[t] + div
        rets <- c(0, x_[-1L]/x_[-n] - 1)
        new.series <- x[1L] * cumprod(1 + rets)
        if (backward)
            new.series <- new.series * x[n] / new.series[n]
    } else {
        dif <- c(0, x[-1L] - x[-n])
        dif[t] <- dif[t] + div
        new.series <- x[1L] + cumsum(dif)
        if (backward)
            new.series <- new.series - new.series[n] + x[n]
    }
    new.series
}

split_adjust <- function(x, t, ratio, backward = TRUE) {

    ## TODO  the function should work for
    ##       matrices of one column as well
    if (!is.null(dim(x)))
        stop(sQuote("x"), " must be a vector")

    tmp <- t > 1L & t <= length(x)
    if (all(!tmp))
        return(x)

    if (length(t) > 1L && length(ratio) == 1L)
        ratio <- rep(ratio, length(t))

    ratio <- ratio[tmp]
    t <- t[tmp]

    if (length(ratio) != length(t))
        stop("different lengths for ", sQuote("ratio"),
             " and ", sQuote("t"))
    new.series <- x
    for (s in t) {
        t1 <- seq_len(s - 1L)
        new.series[t1] <- new.series[t1]/ratio[s == t]
    }
    if (!backward)
        new.series <- x[1L] * new.series/new.series[1L]
    new.series
}
