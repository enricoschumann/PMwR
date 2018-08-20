## -*- truncate-lines: t; -*-
## Copyright (C) 2008-18  Enrico Schumann

unit_prices <- function(NAV, cashflows,
                        initial.price = 100,
                        initial.shares = 0,
                        cf.included = FALSE) {

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
