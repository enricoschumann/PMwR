## -*- truncate-lines: t; -*-
## Copyright (C) 2008-20  Enrico Schumann

unit_prices <- function(NAV, cashflows,
                        initial.price,
                        initial.shares = 0,
                        cf.included = TRUE) {

    ## result 'price' series
    ## + attr (data.frame) transactions
    ##                     - timestamp
    ##                     - id
    ##                     - shares
    ##                     - price

    if (!cf.included)
        .NotYetUsed("cf.included")
    if (initial.shares != 0)
        .NotYetUsed("initial.shares")

    if (inherits(NAV, "zoo"))
        NAV <- data.frame(index(NAV), NAV)

    if (inherits(cashflows, "zoo"))
        cashflows <- data.frame(index(cashflows), cashflows)

    if (!cf.included) {

    }
    
    shares <- numeric(nrow(cashflows))
    all.t <- sort(unique(cashflows[[1]]))
    t.NAV <- match(all.t, NAV[[1]])

    if (any(is.na(t.NAV)))
        stop("cashflow without matching NAV timestamp")

    price <- numeric(length(all.t))
    if (initial.shares == 0) {
        if (missing(initial.price))
            initial.price <- 100
        price[1] <- initial.price
    } else
        price[1] <- (NAV[t.NAV[1]] - cf.included*sum(cashflows[[2]][1]))/
            initial.shares
    shares[all.t[1] == cashflows[[1]]] <- cashflows[[2]][cashflows[[1]] == all.t[1]]/price[1] +
        initial.shares
    if (length(all.t) > 1L) {
        sum.shares <- sum(shares[all.t[1] == cashflows[[1]]])
        for (t in all.t[-1]) {
            i <- t == all.t
            price[i] <- (NAV[[2]][t.NAV[i]] -
                         cf.included*sum(cashflows[[2]][cashflows[[1]] == t]))/
                sum.shares
            shares[t == cashflows[[1]]] <- cashflows[[2]][cashflows[[1]] == t]/price[i]
            sum.shares <- sum.shares + sum(shares[t == cashflows[[1]]])
        }
    }

    total.shares <- numeric(nrow(NAV))
    total.shares[t.NAV] <- tapply(shares, cashflows[[1]], sum)
    total.shares <- cumsum(total.shares)

    res <- NAV
    names(res) <- c("timestamp", "NAV")
    res <- cbind(res,
                 data.frame(price = NAV[[2]]/total.shares,
                            units = total.shares,
                            stringsAsFactors = FALSE))

    colnames(cashflows) <- if (ncol(cashflows) == 3L)
                               c("timestamp", "cashflow", "account")
                           else
                               c("timestamp", "cashflow")
    attr(res, "transactions") <- cbind(cashflows,
                                       units = shares)
    res
}

.unit_prices <- function(NAV, cashflows,
                        initial.price = 100,
                        initial.shares = 0,
                        cf.included = FALSE) {

    .Deprecated("unit_prices",
                msg = paste0(sQuote(".unit_prices"), " is deprecated.\n",
                             "Use ", sQuote("unit_prices"), " instead."))
    
    if (initial.shares != 0)
        .NotYetUsed("initial.shares")
    res <- NAV
    names(res) <- c("timestamp", "NAV")
    res <- cbind(res,
                 data.frame(price = NA,
                            shares = 0,
                            cashflow = 0,
                            new_shares = 0,
                            total_shares = 0,
                            NAV_after_cf = NA,
                            stringsAsFactors = FALSE))
    ii <- match(cashflows[[1]], res[[1]])
    if (any(is.na(ii)))
        stop("cashflow without matching NAV timestamp")
    res[["cashflow"]][ii] <- cashflows[[2]]


    if (res[["NAV"]][[1]] - cf.included * res[["cashflow"]][[1]] == 0)
        res[["price"]][[1]] <- initial.price


    for (i in seq_len(nrow(res))) {

        if (i > 1)
            res[["shares"]][[i]] <- res[["total_shares"]][[i-1]]

        if (res[["shares"]][[i]] > 0)
            res[["price"]][[i]]    <- (res[["NAV"]][[i]] -
                                       cf.included * res[["cashflow"]][[i]]) /
                                      res[["shares"]][[i]]
        else if (i > 1)
            res[["price"]][[i]]    <- res[["price"]][[i-1]]


        res[["new_shares"]][[i]]   <- res[["cashflow"]][[i]] /
                                      res[["price"]][[i]]
        res[["total_shares"]][[i]] <- res[["shares"]][[i]] +
                                      res[["new_shares"]][[i]]
        res[["NAV_after_cf"]][[i]] <- res[["total_shares"]][[i]] *
                                      res[["price"]][[i]]
    }
    res
}
