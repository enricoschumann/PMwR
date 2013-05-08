drawdown <- function(v, relative = TRUE) {
    cv  <- cummax(v)
    rd  <- cv - v
    if (relative)
        rd  <- rd/cv
    troughTime <- which.max(rd)
    peakTime <- which.max(v[seq_len(troughTime)])
    list(maximum = max(rd),
         high = v[peakTime],
         highPosition = peakTime,
         low = v[troughTime],
         lowPosition = troughTime)
}

## various functions to compute returns
returns <- function(x, pad = NULL) {
    n <- NROW(x)
    do.pad <- !is.null(pad)
    if (is.null(dim(x))) {
        x <- as.vector(x)
        rets <- x[-1L]/x[-n] - 1
        if (do.pad)
            rets <- c(pad, rets)
    } else {
        x <- as.matrix(x)
        rets <- x[2:n, ] / x[seq_len(n - 1L), ] - 1
        if (do.pad)
            rets <- rbind(pad, rets)
    }
    rets
}
twReturns <- function(price, amount, pad = NULL) {
    if (missing(amount))
        returns(price, pad)
    do.pad <- !is.null(pad)        
    amount <- as.matrix(amount)
    price <- as.matrix(price)
    n <- dim(price)[1L]
    ap <- amount*price
    rt <- returns(price)        
    weights <- (price*amount/rowSums(price*amount))[-n, , drop = FALSE]
    rt <- rowSums(rt*weights)        
    if (do.pad) 
        rt <- c(pad, rt)
    rt
}
pReturns <- function(x, t = NULL, period) {

    ## TODO: make internal function that checks x and t
    if (is.null(t)) {
        if (!inherits(x, "zoo")) {
            stop(sQuote("t"), " not supplied, so ", sQuote("x"),
                 " must inherit from ", sQuote("zoo"))
        } else {
            t <- index(x)
            x <- coredata(x)
        }
    } else {
        if (is.unsorted(t)) {
            idx <- order(t)
            t <- t[idx]
            x <- x[idx]
        }
    }
    if (grep("da(y|i)", period, ignore.case = TRUE)) {
        by <- format(t, "%Y%m%d")
        PMwR:::last(x, by)
    }

}
