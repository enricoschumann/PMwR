PL <- function(notional, prices, symbols = NULL) {
    tol <- 1e-12
    noPL <- FALSE
    if (!is.null(symbols))
        warning("symbols is not supported yet -- it is ignored")
    if (any(abs(notional) < tol))
        warning("zero notional")
    if (abs(sum(notional)) > tol) {
        warning("Sum of notional is not zero; cannot compute PnL.")
        noPL <- TRUE
    }
    sel <- notional < 0L
    buy <- notional > 0L
    abuy  <- sum(prices[buy] * notional[buy]) / sum(notional[buy])
    asel <- sum(prices[sel] * notional[sel]) / sum(notional[sel])
    PL <- (asel - abuy)*sum(notional[buy])
    list(averagePriceBuy  = ifelse(is.finite(abuy), abuy, NA),
         averagePriceSell = ifelse(is.finite(asel), asel, NA),
         PLtotal = ifelse(noPL, NA, PL),
         PLperContract = ifelse(noPL, NA, PL/sum(notional[buy])),
         absNotional = sum(abs(notional)))
}
PLsorted <- function(notional, prices,
                     tradetimes = NULL,
                     allprices = NULL, alltimes = NULL,
                     initcash = 0,
                     do.sort = FALSE) {
    if ((n <- length(notional)) != length(prices))
        stop("length(notional) != length(prices)")

    if (any(notional == 0))
        stop("'notional' must be nonzero")

    if (is.null(tradetimes)) {
        cumcash <- cumsum(-prices * notional)
        cumpos <- cumsum(notional)
        list(wealth = cumpos * prices + cumcash,
             position = cumpos)
    } else {
        if (do.sort) {
            ## trade data
            ot <- order(tradetimes)
            prices <- prices[ot]
            notional <- notional[ot]
            tradetimes <- tradetimes[ot]
            ## overall series
            ot <- order(alltimes)
            alltimes <- alltimes[ot]
            allprices <- allprices[ot]
        }

        ## (0) aggregate notional in case of duplicated times
        if (any(duplicated(tradetimes))) {

            ## diffsigns checks whether the signs of quantities differ
            diffsigns <- function(x)
                if (length(x) > 1L && length(unique(sign(x))) > 1L)
                    TRUE else FALSE

            instTrade <- aggregate(notional, list(tradetimes), diffsigns)
            if (any(instTrade[["x"]])) {

                ## if there were trade in a single instance of
                ## time: loop over those periods and add results
                ## to cash
                nInstTrade <- sum(instTrade[["x"]])
                iInstTrade <- which(instTrade[["x"]])
                addedCash <- numeric(nInstTrade)
                addedTime <- vector(mode = mode(tradetimes),
                                    length = nInstTrade)

                for (i in seq_len(nInstTrade)) {
                    this.t <- instTrade[[1L]][iInstTrade[i]]
                    this.rows <- which(tradetimes == this.t)
                    this.prices <- prices[this.rows]
                    this.notional <- notional[this.rows]

                    sells <- this.notional < 0
                    buys  <- this.notional > 0
                    sumsell <- sum(abs(this.notional[sells]))
                    sumbuy  <- sum(abs(this.notional[buys]))

                    abstradesize <- min(sumsell, sumbuy)
                    this.adj <- numeric(length(this.notional))
                        this.adj <- -this.notional
                    if (sumsell < sumbuy) {
                        this.adj[buys] <- -this.notional[buys]*sumsell/sumbuy
                    } else {
                        this.adj[sells] <- -this.notional[sells]*sumbuy/sumsell
                    }

                    addedCash[i] <- PL(-this.adj, this.prices)$PLtotal
                    addedTime[i] <- this.t

                    ## remove closed trades
                    notional[this.rows] <- notional[this.rows] +
                        this.adj

                }
            }

            tmpnotional <- aggregate(notional, list(tradetimes), sum)
            tmpprices <- aggregate(prices, list(tradetimes), tail,1)
            prices <- aggregate(notional * prices, list(tradetimes),
                                sum)[["x"]]/
                                    ifelse(abs(tmpnotional[["x"]]) < 1e-12,
                                           1, tmpnotional[["x"]])
            if (any(repp <- tmpnotional[[2L]] == 0L))
                prices[repp] <- tmpprices[[2L]][repp]

            notional <- tmpnotional[["x"]]
            tradetimes <- tmpnotional[["Group.1"]]
        }
        ## (1) add missing times: checks if all tradetimes are included
        ##                        in alltimes. If not, add the missing
        ##                        times and prices.
        tmatch <- match(tradetimes, alltimes)
        if (any(is.na(tmatch))) {
            alltimes <- c(alltimes, tradetimes[is.na(tmatch)])
            ot <- order(alltimes)
            alltimes <- alltimes[ot]
            allprices <- c(allprices, prices[is.na(tmatch)])[ot]
            tmatch <- match(tradetimes, alltimes) ## match again
        }

        ## (2) replace prices: use actual trade prices for valuation
        allprices[tmatch] <- prices


        ## set up cash
        cash <- rep(0, length(allprices))
        cash[1L] <- initcash

        position <- numeric(length(alltimes))
        position[tmatch] <- notional
        cash[tmatch] <- cash[tmatch] - allprices[tmatch] * position[tmatch]

        ## add instantaneuous trades
        if (exists("addedTime")) {
            itmp <- match(addedTime, alltimes)
            cash[itmp] <- cash[itmp] + addedCash
        }
        cumcash <- cumsum(cash)
        list(time = alltimes,
             prices = allprices,
             notional = position,
             position = cumsum(position),
             cash = cash,
             cashposition = cumcash,
             wealth = cumcash + cumsum(position) * allprices)
    }
}
splitTrades <- function(notional, prices, tradetimes, aggregate = FALSE) {
    n <- notional
    p <- prices

    cumn <- cumsum(n)
    N <- length(cumn)

    I <- which(cumn[-1L] * cumn[-N] < 0) + 1L
    if (length(I)) {
        n[I] <- -cumn[I - 1L]
        newtimes <- tradetimes[I]
        newn <- cumn[I]
        newp <- p[I]
        n <- c(n, newn)
        p <- c(p, newp)
        tradetimes <- c(tradetimes, newtimes)
        ix <- order(tradetimes)
        tradetimes <- tradetimes[ix]
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
                                 tradetimes = tradetimes[fromto])
            }
        } else {
            ## there is only one trade, and it is still open
            res <- list(n = n, p = p, tradetimes = tradetimes)
        }
        res
    } else {
        list(n = n, p = p, tradetimes = tradetimes)
    }
}

scaleTrades <- function(notional, prices, tradetimes, aggregate = FALSE,
                        fun = NULL, ...) {
    n <- notional
    p <- prices
    trades <- splitTrades(n, p, tradetimes, aggregate = FALSE)
    if (is.null(fun))
        fun <- scaleToUnity
    for (i in seq_along(trades)) {
        trades[[i]]$n <- fun(trades[[i]]$n, ...)
        zero <- trades[[i]]$n == 0
        trades[[i]]$n <- trades[[i]]$n[!zero]
        trades[[i]]$p <- trades[[i]]$p[!zero]
        trades[[i]]$tradetimes <- trades[[i]]$tradetimes[!zero]
    }

    if (!aggregate)
        trades
    else {
        n <- unlist(lapply(trades, `[[`, "n"))
        p <- unlist(lapply(trades, `[[`, "p"))
        tradetimes <- unlist(lapply(trades, `[[`, "tradetimes"))
        n <- aggregate(n, list(tradetimes), sum)[["x"]]
        p <- aggregate(p, list(tradetimes), tail,1L)[["x"]]
        list(n = n, p = p, tradetimes = tradetimes) }
}

scaleToUnity <- function(n) {
    maxn <- max(abs(cumsum(n)))
    n/maxn
}
closeOnFirst <- function(n) {
    s <- sign(n)
    s1 <- s[1L]
    cn <- cumsum(n)
    close <- suppressWarnings(min(which(s1 != s)))
    if (is.finite(close)) {
        n[close] <- -cn[close - 1L]
        if (close < length(n))
            n[(close + 1L):length(n)] <- 0L
    }
    n
}

## limitExposure <- function(trade, lim) {
##     tol <- 1e-8
##     skipped <- pos <- 0

##     for (i in seq_along(n)) {
##         npos <- pos + n[i]
##         if (abs(npos) > lim) {
##             nnew    <- (lim - pos) * sign(n[i])
##             skipped <- sign(n[i]) * (n[i] - nnew) + skipped
##             n[i] <- nnew
##             pos <- pos + n[i]
##         } else if (abs(skipped) > tol && sign(n[i]) != sign(skipped)) {
##             if (sign(skipped) > 0) {
##                 nnew <- max(abs(n[i])- abs(skipped), 0) * sign(n[i])
##                 skipped <- skipped - sign(n[i]) * (n[i] - nnew)
##                 n[i] <- nnew
##                 pos <- pos + n[i]
##             }
##         } else
##             pos <- npos
##     }
##     keep <- abs(n) >= tol
##     trade$n <- n[keep]
##     trade$p <- trade$p[keep]
##     trade$tradetimes <- trade$tradetimes[keep]
##     trade
## }
limit <- function(trade, lim, tol = 1e-8) {
    n <- trade[["n"]]
    cn <- cumsum(n)
    if (cn[1L] > 0L)
        cnL <- pmin(cn, lim) else cnL <- pmax(cn, -lim)
    nL <- diff(c(0, cnL))
    subset <- which(abs(nL) >= tol)
    list(n = nL[subset], p = trade$p[subset],
         tradetimes = trade$tradetimes[subset])
}


## tests
## n <- 0
## scaleToUnity(0)
## closeOnFirst(0)
## splitTrades(c(1,-3,2), p = c(100,104,102), 1:3, FALSE)
## scaleTrades(c(1,-3,2), p = c(100,104,102), 1:3, FALSE, fun = scaleToUnity)
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
monthlyStats <- function(x, t = NULL) {
    if (!is.null(dim(x))&&dim(x)[2L]>1L)
        stop("'x' must be a _vector_ of class zoo")
    if (!is.zoo(x))
        stop("'x' must be a vector of class _zoo_")

    returns <- function(x)
        diff(x)/x[-1L]

    x.eom <- aggregate(x, strftime(index(x), "%Y%m"), tail,1)
    ult.dates <- endOfMonth(index(x))
    x.eom <- zoo(x.eom, unique(ult.dates))

    if (identical(ult.dates[1L], ult.dates[2L])) {
        tmp <- zoo(coredata(x[1L]), endOfPreviousMonth(ult.dates[1L]))
        x.eom <- rbind(tmp, x.eom)
    }
    res <- list(index = x.eom/coredata(x.eom[1L]),
                returns = returns(x.eom))
}
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
        rets <- x[2:n, ] / x[seq_len(n-1), ] - 1
        if (do.pad)
            rets <- rbind(pad, rets)
    }
    rets
}
