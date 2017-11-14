## -*- truncate-lines: t; -*-

div_adjust <- function(x, t, div, backward = TRUE, additive = FALSE) {
    if (!is.null(dim(x)))
        stop(sQuote("x"), " must be a vector")
    if (length(div) == 1L && length(t) > 1L)
        div <- rep(div, length(t))
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

split_adjust <- function(x, t, ratio, backward = TRUE) {
    if (!is.null(dim(x)))
        stop(sQuote("x"), " must be a vector")
    tmp <- t > 1L
    ratio <- ratio[tmp]
    t <- t[tmp]
    if (length(ratio) != length(t))
        stop("different lengths for ", sQuote("ratio"),
             " and ", sQuote("t"))
    n <- length(x)
    new.series <- x
    for (s in t) {
        t1 <- seq_len(t-1L)
        new.series[t1] <- x[t1]/2
    }
    if (!backward)
        new.series <- x[1L] * new.series/new.series[1L] 
    new.series        
}
