PL <- function(notional, prices, symbols = NULL, tol = 1e-10, fast = FALSE) {
    if (fast) {
        if (length(notional) > 1000L)
            p <- -drop(crossprod(notional, prices)) else
        p <- -sum(notional * prices)
        asum <- sum(abs(notional))
        list(averagePriceBuy  = NA,
             averagePriceSell = NA,
             PLtotal = p,
             PLperContract = p/asum*2,
             absNotional = asum)        
    } else {        
        if (any(abs(notional) < tol))
            warning("zero notional")
        PLfun <- function(notional, prices, tol) {
            if (abs(sum(notional)) > tol) {
                warning("Sum of notional is not zero; cannot compute PnL.")
                noPL <- TRUE
            } else
                noPL <- FALSE
            sel  <- notional < 0L   
            buy  <- notional > 0L
            nbuy <- notional[buy] 
            nsel <- notional[sel]
            nbuySum <- sum(nbuy)
            nselSum <- sum(nsel)
            abuy <- sum(prices[buy] * nbuy)/nbuySum
            asel <- sum(prices[sel] * nsel)/nselSum
            PL <- (asel - abuy)*nbuySum
            list(averagePriceBuy  = if (is.finite(abuy)) abuy else NA,
                 averagePriceSell = if (is.finite(asel)) asel else NA,
                 PLtotal = ifelse(noPL, NA, PL),
                 PLperContract = ifelse(noPL, NA, PL/nbuySum),
                 absNotional = nbuySum-nselSum)
        }
        if (is.null(symbols)) {
            PLfun(notional, prices, tol)
        } else {
            slist <- unique(sort(symbols))
            res <- vector("list", length=length(slist))
            names(res) <- slist
            for (s in slist)
                res[[s]] <- PLfun(notional[s == symbols],
                                  prices[  s == symbols], tol)
            res
        }
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

## TODO
monthlyStats <- function(x, t = NULL) {
    if (!is.null(dim(x)) && dim(x)[2L] > 1L)
        stop("'x' must be a vector")

    x.eom <- aggregate(x, strftime(index(x), "%Y%m"), tail, 1)
    ult.dates <- endOfMonth(index(x))
    x.eom <- zoo(x.eom, unique(ult.dates))

    if (identical(ult.dates[1L], ult.dates[2L])) {
        tmp <- zoo(coredata(x[1L]), endOfPreviousMonth(ult.dates[1L]))
        x.eom <- rbind(tmp, x.eom)
    }
    res <- list(index = x.eom/coredata(x.eom[1L]),
                returns = returns(x.eom))
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
    
    i <- PMwR:::last(x, by, TRUE)
    if (length(dim(x)))
        x[i, ]
    else
        x[i]        
}

x <- structure(list(Close = c(172.8415, 172.8522, 172.5594, 172.2736, 
                    171.9424, 172.2876, 172.3818, 173.1075, 174.0463, 174.1346, 174.2846, 
                    174.2394, 174.2856, 174.1445, 174.325, 174.4961, 174.4185, 174.7009, 
                    175.4982, 176.1342, 176.2193, 176.5463, 177.1582, 177.4939, 177.2971, 
                    177.619, 177.3849, 177.3853, 177.3474, 177.436, 177.7182, 177.7722, 
                    177.6369, 177.5458, 177.5367, 177.7228, 177.8751, 178.0071, 178.1798, 
                    178.3554, 178.775, 179.0384, 179.3701, 179.9599, 180.3867, 180.1115, 
                    179.7792, 179.9847, 180.2883, 180.4221, 180.4312, 180.2663, 179.7633, 
                    179.6309, 179.2245, 179.5027, 179.4586, 179.4976, 179.5227, 179.7475, 
                    179.7565, 179.7629, 179.9239, 179.6704, 180.114, 180.1823, 179.9695, 
                    179.4623, 179.3584, 179.1331, 179.2486, 179.5531, 179.4164, 179.1479, 
                    179.2864, 179.495, 179.2202, 178.9568, 178.9591, 179.1079, 179.2968, 
                    179.6068, 179.529, 180.0816, 180.3174, 180.5573, 180.9332, 181.0941, 
                    181.2409, 180.651, 181.0209, 181.0813, 180.8069, 180.3659, 180.2212, 
                    180.3072, 180.3781, 180.4109, 180.8095, 181.0395, 181.6403, 181.5753, 
                    181.3874, 181.4024, 181.3631, 181.9634, 182.5783, 182.6583, 182.6537, 
                    182.1057, 181.6295, 181.6836, 181.2364, 180.0043, 179.4557, 179.5646, 
                    180.3937, 180.1881, 179.8228, 179.9135, 180.6019, 180.6103, 180.582, 
                    179.8611, 179.6192, 179.6842, 180.5741, 181.2744, 181.6587, 181.7971, 
                    181.7302, 182.2429, 181.8964, 182.3923, 183.0346, 183.2195, 183.1904, 
                    183.3715, 183.6881, 183.6712, 183.8184, 183.6661, 182.8113, 181.9503, 
                    181.9977, 182.9127, 182.993, 183.4738, 183.8638, 183.8076, 183.9668, 
                    183.872, 184.3943, 184.0242, 184.3768, 184.5338, 184.8123, 184.6591, 
                    184.5482, 184.2359, 184.5327, 184.838, 185.0388, 184.929, 185.3638, 
                    185.6718, 185.7699, 185.8161, 185.8084, 185.6074, 185.6944, 185.4684, 
                    185.3695, 185.7588, 185.7793, 185.6964, 186.4902, 186.1581, 186.465, 
                    186.3217, 186.6538, 185.9819, 185.9249, 186.269, 186.6686, 186.8048, 
                    186.7644, 187.0052, 186.6656, 186.9148, 187.0398, 187.2569, 187.1888, 
                    187.3971, 187.4669, 187.5096, 187.5639, 187.8146, 187.8336, 187.8759, 
                    188.0242, 188.477, 188.4385, 188.1602, 188.1821, 188.3185, 188.5074, 
                    188.3451, 188.244, 188.35, 188.2849, 188.4443, 188.5905, 188.6335, 
                    188.797, 188.927, 189.0701, 189.1221, 189.4071, 189.6637, 189.5966, 
                    189.799, 189.842, 189.9775, 189.855, 189.9253, 190.0093, 189.9274, 
                    189.6153, 189.5858, 189.7158, 189.811, 190.011, 190.1522, 190.9173, 
                    190.9059, 191.0117, 191.0651, 191.2875, 191.4758, 191.5761, 191.6843, 
                    191.2045, 191.3785, 191.565, 191.6346, 191.737, 191.692, 191.7753, 
                    191.9455, 192.1431, 192.2128, 192.4234, 192.5097)),
               .Names = "Close", class = "data.frame",
               row.names = c("2012-01-02", 
               "2012-01-03", "2012-01-04", "2012-01-05", "2012-01-06", "2012-01-09", 
               "2012-01-10", "2012-01-11", "2012-01-12", "2012-01-13", "2012-01-16", 
               "2012-01-17", "2012-01-18", "2012-01-19", "2012-01-20", "2012-01-23", 
               "2012-01-24", "2012-01-25", "2012-01-26", "2012-01-27", "2012-01-30", 
               "2012-01-31", "2012-02-01", "2012-02-02", "2012-02-03", "2012-02-06", 
               "2012-02-07", "2012-02-08", "2012-02-09", "2012-02-10", "2012-02-13", 
               "2012-02-14", "2012-02-15", "2012-02-16", "2012-02-17", "2012-02-20", 
               "2012-02-21", "2012-02-22", "2012-02-23", "2012-02-24", "2012-02-27", 
               "2012-02-28", "2012-02-29", "2012-03-01", "2012-03-02", "2012-03-05", 
               "2012-03-06", "2012-03-07", "2012-03-08", "2012-03-09", "2012-03-12", 
               "2012-03-13", "2012-03-14", "2012-03-15", "2012-03-16", "2012-03-19", 
               "2012-03-20", "2012-03-21", "2012-03-22", "2012-03-23", "2012-03-26", 
               "2012-03-27", "2012-03-28", "2012-03-29", "2012-03-30", "2012-04-02", 
               "2012-04-03", "2012-04-04", "2012-04-05", "2012-04-10", "2012-04-11", 
               "2012-04-12", "2012-04-13", "2012-04-16", "2012-04-17", "2012-04-18", 
               "2012-04-19", "2012-04-20", "2012-04-23", "2012-04-24", "2012-04-25", 
               "2012-04-26", "2012-04-27", "2012-04-30", "2012-05-02", "2012-05-03", 
               "2012-05-04", "2012-05-07", "2012-05-08", "2012-05-09", "2012-05-10", 
               "2012-05-11", "2012-05-14", "2012-05-15", "2012-05-16", "2012-05-17", 
               "2012-05-18", "2012-05-21", "2012-05-22", "2012-05-23", "2012-05-24", 
               "2012-05-25", "2012-05-28", "2012-05-29", "2012-05-30", "2012-05-31", 
               "2012-06-01", "2012-06-04", "2012-06-05", "2012-06-06", "2012-06-07", 
               "2012-06-08", "2012-06-11", "2012-06-12", "2012-06-13", "2012-06-14", 
               "2012-06-15", "2012-06-18", "2012-06-19", "2012-06-20", "2012-06-21", 
               "2012-06-22", "2012-06-25", "2012-06-26", "2012-06-27", "2012-06-28", 
               "2012-06-29", "2012-07-02", "2012-07-03", "2012-07-04", "2012-07-05", 
               "2012-07-06", "2012-07-09", "2012-07-10", "2012-07-11", "2012-07-12", 
               "2012-07-13", "2012-07-16", "2012-07-17", "2012-07-18", "2012-07-19", 
               "2012-07-20", "2012-07-23", "2012-07-24", "2012-07-25", "2012-07-26", 
               "2012-07-27", "2012-07-30", "2012-07-31", "2012-08-01", "2012-08-02", 
               "2012-08-03", "2012-08-06", "2012-08-07", "2012-08-08", "2012-08-09", 
               "2012-08-10", "2012-08-13", "2012-08-14", "2012-08-15", "2012-08-16", 
               "2012-08-17", "2012-08-20", "2012-08-21", "2012-08-22", "2012-08-23", 
               "2012-08-24", "2012-08-27", "2012-08-28", "2012-08-29", "2012-08-30", 
               "2012-08-31", "2012-09-03", "2012-09-04", "2012-09-05", "2012-09-06", 
               "2012-09-07", "2012-09-10", "2012-09-11", "2012-09-12", "2012-09-13", 
               "2012-09-14", "2012-09-17", "2012-09-18", "2012-09-19", "2012-09-20", 
               "2012-09-21", "2012-09-24", "2012-09-25", "2012-09-26", "2012-09-27", 
               "2012-09-28", "2012-10-01", "2012-10-02", "2012-10-03", "2012-10-04", 
               "2012-10-05", "2012-10-08", "2012-10-09", "2012-10-10", "2012-10-11", 
               "2012-10-12", "2012-10-15", "2012-10-16", "2012-10-17", "2012-10-18", 
               "2012-10-19", "2012-10-22", "2012-10-23", "2012-10-24", "2012-10-25", 
               "2012-10-26", "2012-10-29", "2012-10-30", "2012-10-31", "2012-11-01", 
               "2012-11-02", "2012-11-05", "2012-11-06", "2012-11-07", "2012-11-08", 
               "2012-11-09", "2012-11-12", "2012-11-13", "2012-11-14", "2012-11-15", 
               "2012-11-16", "2012-11-19", "2012-11-20", "2012-11-21", "2012-11-22", 
               "2012-11-23", "2012-11-26", "2012-11-27", "2012-11-28", "2012-11-29", 
               "2012-11-30", "2012-12-03", "2012-12-04", "2012-12-05", "2012-12-06", 
               "2012-12-07", "2012-12-10", "2012-12-11", "2012-12-12", "2012-12-13", 
               "2012-12-14", "2012-12-17", "2012-12-18", "2012-12-19", "2012-12-20", 
               "2012-12-21", "2012-12-27", "2012-12-28"))

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

twExposure <- function(notional, tradetimes, start, end, abs.value = TRUE) {
    if (missing(start))
        start <- min(tradetimes)
    else {
        tradetimes <- c(start, tradetimes)
        notional <- c(0, notional)
    }
    if (missing(end))
        end <- max(tradetimes)
    else {
        tradetimes <- c(tradetimes, end)
        notional <- c(notional,0)
    }

    n <- cumsum(notional)[-length(notional)]
    if (abs.value)
        n <- abs(n)
    sum(n * diff(as.numeric(tradetimes)))/
        (as.numeric(end)-as.numeric(start))
}


## n <- c(2,-2,-1,1)
## cumsum(n)

## st <- Sys.time()
## tradetimes <- c(st-20, st, st+10, st+30)
## twExposure(n, tradetimes)
## twExposure(n, tradetimes, st-60)
## twExposure(n, tradetimes, end=st+100)



PLsorted <- function(x, ...) {
    UseMethod("PLsorted")
}
PLsorted.Tradelist <- function(x, allprices = NULL, alltimes = NULL,
                               initcash = 0, do.sort = FALSE) {
    allinstr <- unique(x$instrument)
    ans <- vector("list", length = length(allinstr))
    for (i in seq_along(allinstr)) {
        ii <- allinstr[i] == x$instrument
        position <- cumsum(x$notional[ii])
        pf <- x$price[ii] * position
        wealth <- pf + cumsum(-x$notional[ii] * x$price[ii]) + initcash        
        ans[[i]] <- list(position = position, wealth = wealth)
    }
    names(ans) <- allinstr
    ans
}
PLsorted.default <- function(notional, prices,
                     tradetimes = NULL,
                     allprices = NULL, alltimes = NULL,
                     initcash = 0, do.sort = FALSE) {
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
