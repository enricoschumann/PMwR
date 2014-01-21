## -*- truncate-lines: t; -*-
## Time-stamp: <2014-01-21 10:22:17 CET (es)>

## x <- c(9,9,10,8,10,11)
## div <- 2
## t <- 4

divAdjust <- function(x, t, div, backward = TRUE, additive = FALSE) {
    if (!is.null(dim(x)))
        stop(sQuote("x"), " must be a vector")
    tmp <- t > 1L
    div <- div[tmp]
    t <- t[tmp]
    if (length(t) > 1L && length(div) == 1L)
        div <- rep(div, length(t))
    else if (length(div) != length(t))
        stop("different lengths for ", sQuote("div"),
             " and ", sQuote("t"))
    n <- length(x)
    if (!additive) {
        rets <- c(0, x[-1L]/x[-n] - 1)
        rets[t] <- (x[t] + div)/x[t - 1L] - 1
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
