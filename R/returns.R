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
pReturns <- function(x, t = NULL, period, complete.first = TRUE) {

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
    if (grepl("da(y|i)", period, ignore.case = TRUE)) {
        by <- format(t, "%Y%m%d")
    } else if (grepl("month", period, ignore.case = TRUE)) {
        by <- format(t, "%Y%m")
    }
    ii <- PMwR:::last(x, by, TRUE)
    if (complete.first && by[1L] == by[2L])
        ii <- c(1, ii)

    ans <- list(returns = returns(x[ii]),
                t = t[ii][-1L],
                period = period)

}

require("tseries")
sp   <- get.hist.quote("^GSPC", quote="AdjClose")
  
  
mReturns <- function(x, t = NULL, complete.first = TRUE) {
    if (is.null(t) && !inherits(x, "zoo"))
        stop("x should be of class zoo")
    
    Ym <- format(index(x), "%Y%m")
    
    if (complete.first && Ym[1] == Ym[2])
        index(x)[1L] <- endOfPreviousMonth(index(x)[1L])
    rets <- PMwR:::last(sp, Ym)
    
}

monthlyStats <- function(t,x) {
    x.eom <- aggregate(x, strftime(index(x), "%Y%m"), tail,1)
    ult.dates <- getEndOfMonth(index(x))
    x.eom <- zoo(x.eom, unique(ult.dates))
    
    if (identical(ult.dates[1L], ult.dates[2L])) {
        tmp <- zoo(coredata(x[1L]), getEndOfPreviousMonth(ult.dates[1L]))
        x.eom <- rbind(tmp, x.eom)
    }
    list(index = x.eom/coredata(x.eom[1L]),
         returns = zoo(returns(x.eom), index(x.eom)[-1L]))
}
rtTab <- function(x) {
    f <- function(x)
        format(round(coredata(100*x), 1), nsmall=1)
    years <- as.numeric(format(index(x), "%Y"))
    mons <- as.numeric(format(index(x), "%m"))
    tb <- array("", dim = c(length(unique(years)), 14L))
    tb[cbind(years - years[1L] + 1L, mons + 1L)] <- f(x)
    for (y in sort(unique(years)))
        tb[y - years[1L] + 1L, 14L] <- f(prod(coredata(x)[years==y] + 1L) - 1L)
    tb[ ,1L] <- sort(unique(years))
    tb
    paste(apply(tb, 1, function(x) paste(x, collapse = "&")), "\\\\")
}
