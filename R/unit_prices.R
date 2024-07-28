## -*- truncate-lines: t; -*-
## Copyright (C) 2008-24  Enrico Schumann

unit_prices <- function(NAV, cashflows,
                        initial.price,
                        initial.units = 0,
                        cf.included = TRUE,
                        round.price = NULL,
                        round.units = NULL) {

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
    units <- numeric(nrow(cashflows)) + NA
    S <- initial.units

    t.NAV <- match(T, NAV[[1]])
    if (any(is.na(t.NAV))) {
        stop("cashflow without matching NAV timestamp: ",
             paste(cashflows[[1]][is.na(t.NAV)], collapse = ", "))
    }



    ## rounding
    do.round.price <- !is.null(round.price)
    if (is.numeric(round.price)) {
        fun_price <- function(x, amount = NULL, ...)
            round(x, round.price)
    } else
        fun_price <- round.price

    ## rounding
    do.round.units <- !is.null(round.units)
    if (is.numeric(round.units)) {
        fun_units <- function(x, amount = NULL, ...)
            round(x, round.units)
    } else
        fun_units <- round.units


    if (missing(initial.price) && initial.units == 0)
        initial.price <- 100

    for (t in T) {
        cf.t <- cashflows[[1L]] == t
        scf <- sum(cashflows[[2]][cf.t])
        if (t == T[1L] && initial.units == 0)
            p <- initial.price
        else
            p <- (NAV[[2L]][t.NAV[t == T]] - cf.included*scf)/S

        if (do.round.price)
            p  <- fun_price(p,  cashflows[[2]][cf.t])

        dS <- cashflows[[2]][cf.t]/p
        if (do.round.units)
            dS <- fun_units(dS, cashflows[[2]][cf.t])

        units[cf.t] <- dS
        price[t == T] <- p

        S <- S + sum(dS)
    }

    total.units <- numeric(nrow(NAV))
    total.units[t.NAV] <- tapply(units, cashflows[[1L]], sum)
    total.units[1L] <- total.units[1L] + initial.units
    total.units <- cumsum(total.units)

    res <- NAV
    colnames(res) <- c("timestamp", "NAV")
    p <- NAV[[2L]]/total.units
    p[t.NAV] <- price
    res <- cbind(res,
                 data.frame(price = p,
                            units = total.units,
                            stringsAsFactors = FALSE))

    colnames(cashflows) <- if (ncol(cashflows) == 3L)
                               c("timestamp", "cashflow", "account")
                           else
                               c("timestamp", "cashflow")
    attr(res, "transactions") <- cbind(cashflows,
                                       units = units)
    res
}
