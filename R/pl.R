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

pl <- function(amount, price, instrument = NULL, timestamp = NULL,
               along.timestamp = FALSE,
               do.sort = FALSE,
               initcash = 0,
               t0, t1, prices0, prices1,
               tol = 1e-10, do.warn = TRUE) {
    if (inherits(amount, "journal")) {
        J <- amount
        price <- J$price
        instrument <- J$instrument
        amount <- J$amount
        timestamp <- J$timestamp
    }
    if (any(abs(amount) < tol) && do.warn)
        warning("zero ", sQuote("amount"), " values")

    if (do.sort) {
        if (is.null(timestamp))
            warning("cannot sort without timestamp")
        else  {
            ot <- order(timestamp)
            price <- price[ot]
            amount <- amount[ot]
            timestamp <- timestamp[ot]
            instrument <- instrument[ot]
        }
    }
    
    plPeriod <- function(journal, t0, t1, prices0, prices1) {
        p0 <- position(journal, when = t0)
        J0 <- journal(instrument = p0$instrument,
                      amount     = as.vector(p0$position),
                      price      = prices0[match(p0$instrument, names(prices0))],
                      timestamp = t0)
        
        p1 <- position(journal, when = t1)
        J1 <- journal(instrument = p1$instrument,
                      amount     = -as.vector(p1$position), ## switch sign
                      price      = prices1[match(p1$instrument, names(prices1))],
                      timestamp = t1)
        
        Jbetween <- subset(journal, timestamp > t0 & timestamp <= t1)
        
        pl(c(J0, Jbetween, J1))    
    } 
    plfun <- function(amount, price) {
        if (abs(sum(amount)) > tol && do.warn) {
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
    
    if (!missing(t0) && !missing(t1) &&
        !missing(prices0) && !missing(prices1)) {
        j <- journal(timestamp, amount, price = price,
                     instrument = instrument)
        return(plPeriod(j, t0, t1, prices0, prices1))        
    }
    
    ui <- unique(instrument)    
    if (is.null(instrument) || length(ui) == 1L) {
        if (along.timestamp) {
            cumcash <- cumsum(-price * amount)
            cumpos  <- cumsum(amount)
            res <- list(value = cumpos * price + cumcash,
                        position = cumpos,
                        cash = cumcash+initcash)
            class(res) <- "plsorted"
        } else {
            tmp <- plfun(amount, price)
            res <- list(instrument = if (is.null(instrument)) NA
                                     else unique(instrument),
                        pl = tmp[1L],
                        total.amount = tmp[2L],
                        average.buy = tmp[3L],
                        average.sell = tmp[4L])
            class(res) <- "pl"
        }        
    } else {
        if (along.timestamp)
            stop("currently only supported for a single instrument")            
        instr <- sort(ui)
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
        class(res) <- "pl"
    }
    res
}
