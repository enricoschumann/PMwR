splitTrades <- function(amount, price, timestamp, aggregate = FALSE) {
    n <- amount
    p <- price

    cumn <- cumsum(n)
    N <- length(cumn)

    I <- which(sign(cumn[-1L]) * sign(cumn[-N]) < 0) + 1L
    if (length(I)) {
        n[I] <- -cumn[I - 1L]
        newtimes <- timestamp[I]
        newn <- cumn[I]
        newp <- p[I]
        n <- c(n, newn)
        p <- c(p, newp)
        timestamp <- c(timestamp, newtimes)
        ix <- order(timestamp)
        timestamp <- timestamp[ix]
        n <- n[ix]
        p <- p[ix]
    }
    if (!aggregate) {
        to <- which(cumsum(n) == 0L)
        if (length(to)) {
            from <- c(1L, to[-length(to)] + 1L)
            ntrades <- length(to)
            if (length(n) != to[ntrades]) {
                warning("last trade incomplete")
                from <- c(from, to[ntrades] + 1L)
                to <- c(to, length(n))
                ntrades <- ntrades + 1L
            }
            res <- vector(mode = "list", length = ntrades)
            for (i in seq_len(ntrades)) {
                fromto <- from[i]:to[i]
                res[[i]] <- list(n = n[fromto], p = p[fromto],
                                 timestamp = timestamp[fromto])
            }
        } else {
            ## there is only one trade, and it is still open
            res <- list(amount = n, price = p, timestamp = timestamp)
        }
        res
    } else {
        list(amount = n, price = p, timestamp = timestamp)
    }
}
scaleTrades <- function(amount, price, timestamp, aggregate = FALSE,
                        fun = NULL, ...) {
    n <- amount
    p <- price
    trades <- splitTrades(n, p, timestamp, aggregate = FALSE)
    if (is.null(fun))
        fun <- scaleToUnity
    for (i in seq_along(trades)) {
        trades[[i]]$n <- fun(trades[[i]]$n, ...)
        zero <- trades[[i]]$n == 0
        trades[[i]]$n <- trades[[i]]$n[!zero]
        trades[[i]]$p <- trades[[i]]$p[!zero]
        trades[[i]]$timestamp <- trades[[i]]$timestamp[!zero]
    }

    if (!aggregate)
        trades
    else {
        n <- unlist(lapply(trades, `[[`, "n"))
        p <- unlist(lapply(trades, `[[`, "p"))
        timestamp <- unlist(lapply(trades, `[[`, "timestamp"))
        n <- aggregate(n, list(timestamp), sum)[["x"]]
        p <- aggregate(p, list(timestamp), tail,1L)[["x"]]
        list(amount = n, price = p, timestamp = timestamp) }
}
scaleToUnity <- function(amount) {
    maxn <- max(abs(cumsum(amount)))
    amount/maxn
}
closeOnFirst <- function(amount) {
    s <- sign(amount)
    s1 <- s[1L]
    cn <- cumsum(amount)
    close <- suppressWarnings(min(which(s1 != s)))
    if (is.finite(close)) {
        amount[close] <- -cn[close - 1L]
        if (close < length(amount))
            amount[(close + 1L):length(amount)] <- 0L
    }
    amount
}

limit <- function(amount, price, timestamp, lim, tol = 1e-8) {
    cn <- cumsum(amount)
    if (cn[1L] > 0L)
        cnL <- pmin(cn, lim) else cnL <- pmax(cn, -lim)
    nL <- diff(c(0, cnL))
    subset <- which(abs(nL) >= tol)
    list(amount = nL[subset], price = price[subset],
         timestamp = timestamp[subset])
}


## monthlyStats <- function(x, t = NULL) {
##     if (!is.null(dim(x)) && dim(x)[2L] > 1L)
##         stop("'x' must be a vector")

##     x.eom <- aggregate(x, strftime(index(x), "%Y%m"), tail, 1)
##     ult.dates <- endOfMonth(index(x))
##     x.eom <- zoo(x.eom, unique(ult.dates))

##     if (identical(ult.dates[1L], ult.dates[2L])) {
##         tmp <- zoo(coredata(x[1L]), endOfPreviousMonth(ult.dates[1L]))
##         x.eom <- rbind(tmp, x.eom)
##     }
##     res <- list(index = x.eom/coredata(x.eom[1L]),
##                 returns = returns(x.eom))
## }

periodObs <- function(x, t = NULL, period = "month", missing = "NA") {
    if (is.null(t)) {
        if (!inherits(x, "zoo"))
            stop(sQuote("t"), " not supplied, so ", sQuote("x"), 
                 " must inherit from class ", sQuote("zoo"))
        t <- index(x)
        x <- coredata(x)
    }
    if (period == "month")
        by <- strftime(t, "%Y%m")
    else if (period == "day")
        by <- strftime(t, "%Y%m%d")
    else if (period == "year")
        by <- strftime(t, "%Y")
    else if (period == "hour")
        by <- strftime(t, "%Y%m%d%H")
    
    i <- PMwR:::last(x, by, TRUE)
    if (length(dim(x)))
        x[i, ]
    else
        x[i]        
}

twExposure <- function(amount, timestamp, start, end, abs.value = TRUE) {
    if (missing(start))
        start <- min(timestamp)
    else {
        timestamp <- c(start, timestamp)
        amount <- c(0, amount)
    }
    if (missing(end))
        end <- max(timestamp)
    else {
        timestamp <- c(timestamp, end)
        amount <- c(amount,0)
    }

    n <- cumsum(amount)[-length(amount)]
    if (abs.value)
        n <- abs(n)
    sum(n * diff(as.numeric(timestamp)))/
        (as.numeric(end)-as.numeric(start))
}
