## -*- truncate-lines: t; -*-
## Copyright (C) 2008-24  Enrico Schumann

unit_prices <- function(NAV, cashflows,
                        initial.price,
                        initial.units = 0,
                        cf.included = TRUE) {

    if (inherits(NAV, "zoo"))
        NAV <- data.frame(timestamp = index(NAV), NAV)

    if (inherits(cashflows, "zoo"))
        cashflows <- data.frame(timestamp = index(cashflows),
                                cashflows)

    if (any(anyDuplicated(NAV[[1]])))
        warning("NAV series has duplicate timestamps")

    if (is.unsorted(NAV[[1]])) {
        NAV <- NAV[order(NAV[[1]]), , drop = FALSE]
    }

    T <- sort(unique(cashflows[[1L]]))
    price <- numeric(length(T))
    shares <- numeric(nrow(cashflows)) + NA
    S <- initial.units

    t.NAV <- match(T, NAV[[1]])
    if (any(is.na(t.NAV))) {
        stop("cashflow without matching NAV timestamp: ",
             paste(cashflows[[1]][is.na(t.NAV)], collapse = ", "))
    }

    if (missing(initial.price) && initial.units == 0)
        initial.price <- 100

    for (t in T) {
        cf.t <- cashflows[[1L]] == t
        scf <- sum(cashflows[[2]][cf.t])
        if (t == T[1L] && initial.units == 0)
            ## FIXME if NAV.t[1] != cf.t[1] , should
            ## the initial unit-price be adjusted to
            ## reflect the performance of the NAV?
            p <- initial.price
        else
            p <- (NAV[[2L]][t.NAV[t == T]] - cf.included*scf)/S

        dS <- cashflows[[2]][cf.t]/p
        shares[cf.t] <- dS
        price[t == T] <- p

        S <- S + sum(dS)
    }

    total.shares <- numeric(nrow(NAV))
    total.shares[t.NAV] <- tapply(shares, cashflows[[1L]], sum)
    total.shares[1L] <- total.shares[1L] + initial.units
    total.shares <- cumsum(total.shares)

    res <- NAV
    colnames(res) <- c("timestamp", "NAV")
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
