## -*- truncate-lines: t; -*-
## Copyright (C) 2008-18  Enrico Schumann

drawdowns <- function(x, ...)
    UseMethod("drawdowns")

drawdowns.default <- function(x, ...) {

    D <- drawdown(x, summary = FALSE)

    recover <- which(D[-1] == 0 &
                     D[-length(D)] > 0) + 1
    peak    <- which(D[-length(D)] == 0 &
                     D[-1] > 0)

    if ((n <- length(peak)) == 0L)
        return(data.frame(peak = numeric(0),
                          trough = numeric(0),
                          recover = numeric(0),
                          max = numeric(0)))

    trough <- numeric(n)
    
    if (length(recover) < length(peak)) {
        recover <- c(recover, length(x))
        not_recovered <- TRUE
    } else
        not_recovered <- FALSE
    
    for (i in seq_len(n)) {
        subs <- x[peak[i]:recover[i]]
        trough[i] <- peak[i] + which.min(subs) - 1L
    }
    dd <- - x[trough]/x[peak] + 1

    if (not_recovered)
        recover[n] <- NA
    data.frame(peak = peak,
               trough = trough,
               recover = recover,
               max = dd)
}

drawdowns.zoo <- function(x, ...) {
    t <- index(x)
    d <- drawdowns.default(coredata(x))
    d$peak <- t[d$peak]
    d$trough <- t[d$trough]
    d$recover <- t[d$recover]
    d
}

drawdowns.NAVseries <- function(x, ...) {
    xx <- as.zoo(x)
    drawdowns.zoo(xx)
}
