## -*- truncate-lines: t; -*-
## Copyright (C) 2008-21  Enrico Schumann

unit_prices <- function(NAV, cashflows,
                        initial.price,
                        initial.shares = 0,
                        cf.included = TRUE) {

    if (initial.shares != 0)
        .NotYetUsed("initial.shares")

    if (inherits(NAV, "zoo"))
        NAV <- data.frame(index(NAV), NAV)

    if (inherits(cashflows, "zoo"))
        cashflows <- data.frame(index(cashflows), cashflows)

    shares <- numeric(nrow(cashflows))
    all.t <- sort(unique(cashflows[[1L]]))
    t.NAV <- match(all.t, NAV[[1L]])

    if (any(is.na(t.NAV)))
        stop("cashflow without matching NAV timestamp")

    price <- numeric(length(all.t))
    if (initial.shares == 0) {
        if (missing(initial.price))
            initial.price <- 100
        price[1] <- initial.price
    } else
        price[1] <- (NAV[t.NAV[1L]] - cf.included*sum(cashflows[[2L]][1L]))/
            initial.shares
    shares[all.t[1L] == cashflows[[1L]]] <-
        cashflows[[2L]][cashflows[[1L]] == all.t[1L]]/price[1L] +
        initial.shares
    if (length(all.t) > 1L) {
        sum.shares <- sum(shares[all.t[1L] == cashflows[[1L]]])
        for (t in all.t[-1L]) {
            i <- t == all.t
            price[i] <-
                (NAV[[2L]][t.NAV[i]] -
                 cf.included*sum(cashflows[[2L]][cashflows[[1L]] == t]))/
                sum.shares
            shares[t == cashflows[[1L]]] <- cashflows[[2L]][cashflows[[1L]] == t]/price[i]
            sum.shares <- sum.shares + sum(shares[t == cashflows[[1L]]])
        }
    }

    total.shares <- numeric(nrow(NAV))
    total.shares[t.NAV] <- tapply(shares, cashflows[[1L]], sum)
    total.shares <- cumsum(total.shares)

    res <- NAV
    names(res) <- c("timestamp", "NAV")
    p <- NAV[[2L]]/total.shares
    p[t.NAV] <- price
    res <- cbind(res,
                 data.frame(price = p,
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
