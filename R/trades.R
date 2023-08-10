## -*- truncate-lines: t; -*-
## Copyright (C) 2008-23  Enrico Schumann

split_trades <- function(amount, price, timestamp,
                         aggregate = FALSE, drop.zero = FALSE) {
    n <- amount
    if (missing(price))
        price <- rep(NA, length(amount))
    p <- price

    if (missing(timestamp))
        timestamp <- seq_along(amount)
    cumn <- cumsum(amount)
    N <- length(cumn)

    I <- which(sign(cumn[-1L]) * sign(cumn[-N]) < 0) + 1L
    if (length(I)) {
        ## FIXME use textutils::insert?
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
        to <- which(cumsum(n) == 0L)  ## FIXME use tolerance?
        if (length(to)) {
            from <- c(1L, to[-length(to)] + 1L)
            ntrades <- length(to)
            if (length(n) != to[ntrades]) {
                warning("last trade incomplete")
                from <- c(from, to[ntrades] + 1L)
                to <- c(to, length(n))
                ntrades <- ntrades + 1L
            }
            if (drop.zero) {
                n0 <- to - from > 0
                from <- from[n0]
                to   <- to  [n0]
                ntrades <- length(to)
            }

            res <- vector(mode = "list", length = ntrades)
            for (i in seq_len(ntrades)) {
                fromto <- from[i]:to[i]
                res[[i]] <- list(amount = n[fromto],
                                 price  = p[fromto],
                                 timestamp = timestamp[fromto])
            }
        } else {
            ## there is only one trade, and it is still open
            res <- list(list(amount = n, price = p, timestamp = timestamp))
        }
    } else {
        res <- list(amount = n, price = p, timestamp = timestamp)
    }
    res
}

scale_trades <- function(amount, price, timestamp, aggregate = FALSE,
                         fun = NULL, ...) {
    trades <- split_trades(amount, price, timestamp, aggregate = FALSE)
    if (is.null(fun))
        fun <- scale_to_unity
    for (i in seq_along(trades)) {
        trades[[i]]$amount <- fun(trades[[i]]$amount, ...)
        zero <- trades[[i]]$amount == 0
        trades[[i]]$amount <- trades[[i]]$amount[!zero]
        trades[[i]]$price <- trades[[i]]$price[!zero]
        trades[[i]]$timestamp <- trades[[i]]$timestamp[!zero]
    }

    if (!aggregate)
        trades
    else {
        amount <- unlist(lapply(trades, `[[`, "amount"))
        price <- unlist(lapply(trades, `[[`, "price"))
        timestamp <- unlist(lapply(trades, `[[`, "timestamp"))
        amount <- aggregate(amount, list(timestamp), sum)[["x"]]
        price <- aggregate(price, list(timestamp), tail,1L)[["x"]]
        list(amount = amount, price = price, timestamp = timestamp)
    }
}

scale_to_unity <- function(amount) {
    maxn <- max(abs(cumsum(amount)))
    amount/maxn
}

close_on_first <- closeOnFirst <- function(amount) {
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

    i <- last(x, by, TRUE)
    if (length(dim(x)))
        x[i, ]
    else
        x[i]
}

tw_exposure <- twExposure <- function(amount, timestamp,
                                      start, end, abs.value = TRUE) {
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
