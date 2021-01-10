## -*- truncate-lines: t; -*-
## Copyright (C) 2008-21  Enrico Schumann

div_adjust <- function(x, t, div,
                       backward = TRUE,
                       additive = FALSE) {

    ## TODO  the function should work for
    ##       matrices of one column as well
    if (!is.null(dim(x)))
        stop(sQuote("x"), " must be a vector")

    valid.t <- t > 1L & t <= length(x)
    if (all(!valid.t))
        return(x)

    if (length(div) == 1L && length(t) > 1L)
        div <- rep(div, length(t))
    else if (length(div) != length(t))
        stop("different lengths for ",
             sQuote("div"), " and ", sQuote("t"))

    div <- div[valid.t]
    t <- t[valid.t]
    n <- length(x)

    if (anyDuplicated(t)) {
        div <- tapply(div, t, sum)
        t <- as.numeric(names(div))
    }

    if (!additive) {
        rets1 <- c(1, x[-1L]/x[-n])
        rets1[t] <- (x[t] + div)/x[t - 1L]
        new.series <- x[1L] * cumprod(rets1)
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

    valid.t <- t > 1L & t <= length(x)
    if (all(!valid.t))
        return(x)

    if (length(t) > 1L && length(ratio) == 1L)
        ratio <- rep(ratio, length(t))

    ratio <- ratio[valid.t]
    t <- t[valid.t]

    if (length(ratio) != length(t))
        stop("different lengths for ", sQuote("ratio"),
             " and ", sQuote("t"))
    new.series <- x
    for (i in seq_along(t)) {
        t1 <- seq_len(t[i] - 1L)
        new.series[t1] <- new.series[t1]/ratio[i]
    }
    if (!backward)
        new.series <- x[1L] * new.series/new.series[1L]
    new.series
}
