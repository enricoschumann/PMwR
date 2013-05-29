## PLsorted.Journal <- function(x,
##                                allprices = NULL, alltimes = NULL,
##                                initcash = 0, do.sort = FALSE) {
##     allinstr <- unique(x$instrument)
##     ans <- vector("list", length = length(allinstr))
##     for (i in seq_along(allinstr)) {
##         ii <- allinstr[i] == x$instrument
##         position <- cumsum(x$amount[ii])
##         pf <- x$price[ii] * position
##         wealth <- pf + cumsum(-x$amount[ii] * x$price[ii]) + initcash        
##         ans[[i]] <- list(position = position, wealth = wealth)
##     }
##     names(ans) <- allinstr
##     ans
## }
PLsorted <- function(amount, price, 
                     timestamp = NULL,
                     allprices = NULL, alltimes = NULL,
                     initcash = 0, do.sort = FALSE) {
    if ((n <- length(amount)) != length(price))
        stop("length(amount) != length(price)")

    if (any(amount == 0))
        stop("'amount' must be nonzero")

    if (is.null(timestamp)) {
        cumcash <- cumsum(-price * amount)
        cumpos <- cumsum(amount)
        list(wealth = cumpos * price + cumcash,
             position = cumpos)
    } else {
        if (do.sort) {
            ## trade data
            ot <- order(timestamp)
            price <- price[ot]
            amount <- amount[ot]
            timestamp <- timestamp[ot]
            ## overall series
            ot <- order(alltimes)
            alltimes <- alltimes[ot]
            allprices <- allprices[ot]
        }

        ## (0) aggregate amount in case of duplicated times
        if (any(duplicated(timestamp))) {

            ## diffsigns checks whether the signs of quantities differ
            diffsigns <- function(x)
                if (length(x) > 1L && length(unique(sign(x))) > 1L)
                    TRUE else FALSE

            instTrade <- aggregate(amount, list(timestamp), diffsigns)
            if (any(instTrade[["x"]])) {

                ## if there were trade in a single instance of
                ## time: loop over those periods and add results
                ## to cash
                nInstTrade <- sum(instTrade[["x"]])
                iInstTrade <- which(instTrade[["x"]])
                addedCash <- numeric(nInstTrade)
                addedTime <- vector(mode = mode(timestamp),
                                    length = nInstTrade)

                for (i in seq_len(nInstTrade)) {
                    this.t <- instTrade[[1L]][iInstTrade[i]]
                    this.rows <- which(timestamp == this.t)
                    this.price <- price[this.rows]
                    this.amount <- amount[this.rows]

                    sells <- this.amount < 0
                    buys  <- this.amount > 0
                    sumsell <- sum(abs(this.amount[sells]))
                    sumbuy  <- sum(abs(this.amount[buys]))

                    abstradesize <- min(sumsell, sumbuy)
                    this.adj <- numeric(length(this.amount))
                    this.adj <- -this.amount
                    if (sumsell < sumbuy) {
                        this.adj[buys] <- -this.amount[buys]*sumsell/sumbuy
                    } else {
                        this.adj[sells] <- -this.amount[sells]*sumbuy/sumsell
                    }

                    addedCash[i] <- PL(-this.adj, this.price)$PLtotal
                    addedTime[i] <- this.t

                    ## remove closed trades
                    amount[this.rows] <- amount[this.rows] +
                        this.adj

                }
            }

            tmpamount <- aggregate(amount, list(timestamp), sum)
            tmpprice <- aggregate(price, list(timestamp), tail,1)
            price <- aggregate(amount * price, list(timestamp),
                                sum)[["x"]]/
                                    ifelse(abs(tmpamount[["x"]]) < 1e-12,
                                           1, tmpamount[["x"]])
            if (any(repp <- tmpamount[[2L]] == 0L))
                price[repp] <- tmpprice[[2L]][repp]

            amount <- tmpamount[["x"]]
            timestamp <- tmpamount[["Group.1"]]
        }
        ## (1) add missing times: checks if all timestamp are included
        ##                        in alltimes. If not, add the missing
        ##                        times and price.
        tmatch <- match(timestamp, alltimes)
        if (any(is.na(tmatch))) {
            alltimes <- c(alltimes, timestamp[is.na(tmatch)])
            ot <- order(alltimes)
            alltimes <- alltimes[ot]
            allprices <- c(allprices, price[is.na(tmatch)])[ot]
            tmatch <- match(timestamp, alltimes) ## match again
        }

        ## (2) replace price: use actual trade price for valuation
        allprices[tmatch] <- price


        ## set up cash
        cash <- rep(0, length(allprices))
        cash[1L] <- initcash

        position <- numeric(length(alltimes))
        position[tmatch] <- amount
        cash[tmatch] <- cash[tmatch] - allprices[tmatch] * position[tmatch]

        ## add instantaneuous trades
        if (exists("addedTime")) {
            itmp <- match(addedTime, alltimes)
            cash[itmp] <- cash[itmp] + addedCash
        }
        cumcash <- cumsum(cash)
        list(time = alltimes,
             price = allprices,
             amount = position,
             position = cumsum(position),
             cash = cash,
             cashposition = cumcash,
             wealth = cumcash + cumsum(position) * allprices)
    }
}


PL <- function(amount, price, instrument = NULL, tol = 1e-10,
               aggr.accounts = FALSE, account.sep = "::") {
    if (inherits(amount, "Journal")) {
        J <- amount
        price <- J$price
        instrument <- J$instrument
        amount <- J$amount
    }
    if (any(abs(amount) < tol))
        warning("zero 'amount' values")
            
    plfun <- function(amount, price) {
        if (abs(sum(amount)) > tol) {
            warning("Sum of amount is not zero; cannot compute PnL.")
            c(NA, sum(abs(amount)),
              sum(price[ i] * amount[ i])/sum(amount[ i]),
              sum(price[!i] * amount[!i])/sum(amount[!i]))
        } else {
            if (length(amount) > 1000L)
                p <- -drop(crossprod(amount, price)) else
            p <- -sum(amount * price)
            i <- amount > 0
            c(p, sum(abs(amount)),
              sum(price[ i] * amount[ i])/sum(amount[ i]),
              sum(price[!i] * amount[!i])/sum(amount[!i]))
        }
    }
    if (!aggr.accounts && exists("J") && !all(is.na(J$account)) && length(unique(J$account)) > 1L) {
        instrument <- paste0(J$account, account.sep, instrument)
        by.account <- TRUE
    } else 
        by.account <- FALSE

    
    if (is.null(instrument) || length(unique(instrument)) == 1L) {
        tmp <- plfun(amount, price)
        res <- list(instrument = if (is.null(instrument)) NA else instrument,
                    pl = tmp[1L], total.amount = tmp[2L],
                    average.buy = tmp[3L], average.sell = tmp[4L])
    } else {
        instr <- sort(unique(instrument))
        pls <- sumamounts <- mbuys <- msells <- numeric(length(instr))
        for (i in seq_along(instr)) {
            ix <- instr[i] == instrument  
            n <- amount[ix]
            tmp <- plfun(amount[ix], price[ix])
            pls[i] <- tmp[1L]
            sumamounts[i] <- tmp[2L]
            mbuys[i] <- tmp[3L]
            msells[i] <- tmp[4L]
        }
        res <- list(instrument = instr, 
                    pl = pls,
                    total.amount = sumamounts,
                    average.buy = mbuys,
                    average.sell = msells)
    }
    class(res) <- "PL"
    res
}
print.PL <- function(x, ...) {
    oo <- getOption("scipen")
    options(scipen = 1e+08)
    on.exit(options(scipen = oo))
    df <- as.data.frame(unclass(x))
    if (all(is.na(df$instrument))) 
        row.names(df) <- ""
    else 
        row.names(df) <- df[["instrument"]]    
    df <- df[,-1L]
    
    print(df)
    cat("\npl           -- total PnL in units of instrument\n")
    cat("total.amount -- total amount (absolute) of traded instruments\n")
    cat("average.buy  -- average buy price\n")
    cat("average.sell -- average sell price\n")    
    invisible(x)
}



pl <- function(amount, price, instrument = NULL, tol = 1e-10,
               aggr.accounts = FALSE, account.sep = "::") {
    if (inherits(amount, "Journal")) {
        J <- amount
        price <- J$price
        instrument <- J$instrument
        amount <- J$amount
    }
    if (any(abs(amount) < tol))
        warning("zero 'amount' values")
            
    plfun <- function(amount, price) {
        if (abs(sum(amount)) > tol) {
            warning("Sum of amount is not zero; cannot compute PnL.")
            c(NA, sum(abs(amount)),
              sum(price[ i] * amount[ i])/sum(amount[ i]),
              sum(price[!i] * amount[!i])/sum(amount[!i]))
        } else {
            if (length(amount) > 1000L)
                p <- -drop(crossprod(amount, price)) else
            p <- -sum(amount * price)
            i <- amount > 0
            c(p, sum(abs(amount)),
              sum(price[ i] * amount[ i])/sum(amount[ i]),
              sum(price[!i] * amount[!i])/sum(amount[!i]))
        }
    }
    if (!aggr.accounts && exists("J", inherits = FALSE)
        && !all(is.na(J$account)) && length(unique(J$account)) > 1L) {
        instrument <- paste0(J$account, account.sep, instrument)
        by.account <- TRUE
    } else 
        by.account <- FALSE

    
    if (is.null(instrument) || length(unique(instrument)) == 1L) {
        tmp <- plfun(amount, price)
        res <- list(instrument = if (is.null(instrument)) NA else unique(instrument),
                    pl = tmp[1L], total.amount = tmp[2L],
                    average.buy = tmp[3L], average.sell = tmp[4L])
    } else {
        instr <- sort(unique(instrument))
        pls <- sumamounts <- mbuys <- msells <- numeric(length(instr))
        for (i in seq_along(instr)) {
            ix <- instr[i] == instrument  
            n <- amount[ix]
            tmp <- plfun(amount[ix], price[ix])
            pls[i] <- tmp[1L]
            sumamounts[i] <- tmp[2L]
            mbuys[i] <- tmp[3L]
            msells[i] <- tmp[4L]
        }
        res <- list(instrument = instr, 
                    pl = pls,
                    total.amount = sumamounts,
                    average.buy = mbuys,
                    average.sell = msells)
    }
    class(res) <- "pl"
    res
}
print.pl <- function(x, ...) {
    oo <- getOption("scipen")
    options(scipen = 1e+08)
    on.exit(options(scipen = oo))
    df <- as.data.frame(unclass(x))
    if (all(is.na(df$instrument))) 
        row.names(df) <- ""
    else 
        row.names(df) <- df[["instrument"]]    
    df <- df[,-1L]
    print(df)
    cat("\n          pl => total PnL in units of instrument\n")
    cat("total.amount => total /absolute/ amount of traded instruments\n")
    cat(" average.buy => average buy price\n")
    cat("average.sell => average sell price\n")    
    invisible(x)
}



plsorted <- function(amount, price, 
                     timestamp = NULL,
                     initcash = 0, do.sort = FALSE,
                     tol = 1e-10) {

    if ((n <- length(amount)) != length(price))
        stop("length(amount) != length(price)")

    if (any(abs(amount) < tol))
        stop(sQuote("amount"), " must be nonzero (see ",
             sQuote("tol"), " parameter)")

    if (do.sort) {
        if (is.null(timestamp))
            warning("cannot sort without timestamp")
        else  {
            ot <- order(timestamp)
            price <- price[ot]
            amount <- amount[ot]
            timestamp <- timestamp[ot]
        }
    }

    
    cumcash <- cumsum(-price * amount)
    cumpos  <- cumsum(amount)
    list(value = cumpos * price + cumcash,
         position = cumpos)

}
