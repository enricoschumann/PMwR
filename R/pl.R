print.pl <- function(x, ...) {
    oo <- getOption("scipen")
    options(scipen = 1e8)
    on.exit(options(scipen = oo))
    df <- as.data.frame(unclass(x))
    if (all(is.na(df$instrument))) 
        row.names(df) <- ""
    else 
        row.names(df) <- df[["instrument"]]    
    df <- df[ ,-1L]
    df[is.na(df)] <- "."
    print(df, na.print = ".", quote = FALSE, print.gap = 2L)
    cat("\n          pl = total PnL in units of instrument\n")
    cat("total.amount = total /absolute/ amount of traded instruments\n")
    cat(" average.buy = average buy price\n")
    cat("average.sell = average sell price\n")    
    invisible(x)
}

pl <- function(amount, price, instrument = NULL, timestamp = NULL,
               along.timestamp = FALSE,
               do.sort = FALSE,
               initial.cash = 0, initial.position = NULL,
               initial.price, current.price,
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
    
    plfun <- function(amount, price) {
        i <- amount > 0
        if (abs(sum(amount)) > tol && do.warn) {
            warning("sum of amount is not zero; cannot compute profit/loss.")
            c(NA, sum(abs(amount)),
              sum(price[ i] * amount[ i])/sum(amount[ i]),
              sum(price[!i] * amount[!i])/sum(amount[!i]))
        } else {
            if (length(amount) > 1000L)
                p <- -drop(crossprod(amount, price)) else
            p <- -sum(amount * price)
            c(p, sum(abs(amount)),
              sum(price[ i] * amount[ i])/sum(amount[ i]),
              sum(price[!i] * amount[!i])/sum(amount[!i]))
        }
    }

    if (!is.null(initial.position) && length(initial.position$position)) {
        if (!is.null(names(initial.price)))
            initial.price <- initial.price[match(initial.position$instrument,
                                                 names(initial.price))]        
        amount0 <- c(initial.position$position)
        instrument0 <- initial.position$instrument
        timestamp0 <- rep(initial.position$timestamp, length(amount0))
    } else {
        initial.price <- timestamp0 <- amount0 <- numeric(0)
        instrument0 <- character(0)
    }
    amount <- c(amount0, amount)
    timestamp <- c(timestamp0, timestamp)
    instrument <- c(instrument0, instrument)
    price <- c(initial.price, price)
    
    ## open position?
    p1 <- position(amount, timestamp, instrument,
                   drop.zero = TRUE)
    if (length(p1$position) && !missing(current.price)) {
        if (!is.null(names(current.price))) 
            current.price <- current.price[match(p1$instrument, names(current.price))]
        amount1 <- c(-p1$position)  ## revert position
        instrument1 <- p1$instrument
        timestamp1 <- p1$timestamp            
    } else {
        current.price <- timestamp1 <- amount1 <- numeric(0)
        instrument1 <- character(0)            
    }
    amount <- c(amount, amount1)
    timestamp <- c(timestamp, timestamp1)
    instrument <- c(instrument, instrument1)
    price <- c(price, current.price)
    
    
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
