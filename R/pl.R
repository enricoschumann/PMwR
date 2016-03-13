print.pl <- function(x, ..., use.crayon = NULL) {
    ## lapply(x, `[[`, "realised")

    use.crayon <- if (is.null(use.crayon) &&
                      !is.null(tmp <- getOption("PMwR.use.crayon")))
                      tmp else FALSE
    if (!use.crayon)
        bold <- function(x) x
    
    oo <- getOption("scipen")
    options(scipen = 1e8)
    on.exit(options(scipen = oo))

    ni <- length(x)

    numrow <- function(x, w) {
        ans <- substr(paste0(format(x, width = w), collapse = " "),
                      1, ceiling(getOption("width")*0.9))
        if (nchar(ans) < nchar(paste0(format(x, width = w), collapse = " ")))
            ans  <- paste(ans, "[...]")
        ans
    }
    
    for (i in seq_len(ni)) {
        ind <- ""
        if (!all(is.na(attr(x, "instrument")))) {
            cat(attr(x, "instrument")[[i]], "\n")
            ind <- "  "
        }
        w <- max(nchar(prettyNum(c(x[[i]]$pl, x[[i]]$realised, x[[i]]$unrealised))))
        BUY <- if (is.finite(x[[i]]$buy))
                   x[[i]]$buy else "."
        SELL <- if (is.finite(x[[i]]$sell))
                   x[[i]]$sell else "."
        cat(ind, bold("P/L total     "),
            bold(numrow(x[[i]]$pl, w)) , "\n",
            if (any(!is.na(x[[i]]$realised)))
                paste0(ind, "__ realised   ", numrow(x[[i]]$realised, w), "\n"),
            if (any(!is.na(x[[i]]$unrealised)))
                paste0(ind, "__ unrealised ", numrow(x[[i]]$unrealised, w), "\n"),
            ind, "average buy    ", BUY, "\n",
            ind, "average sell   ", SELL, "\n",
            ind, "volume        ",
            numrow(x[[i]]$volume, w), "\n", sep = "")

        if (i < ni)
            cat("\n")
    }
    cat("\n", sQuote("P/L total"), " is in units of instrument;\n",
        sQuote("volume"), " is sum of /absolute/ amounts.\n",
        sep = "")
    invisible(x)
}

## pl <- function(amount, price, instrument = NULL, timestamp = NULL,
##                along.timestamp = FALSE,
##                do.sort = FALSE, 
##                initial.cash = 0, initial.position = NULL,
##                initial.price = NULL, current.price = NULL,
##                multiplier = 1,
##                tol = 1e-10, do.warn = TRUE) {

##     if (inherits(amount, "journal")) {
##         J <- amount
##         price <- J$price
##         instrument <- J$instrument
##         amount <- J$amount
##         timestamp <- J$timestamp
##     }
    
##     if (any(abs(amount) < tol) && do.warn)
##         warning("zero ", sQuote("amount"), " values")

##     if (do.sort) {
##         if (is.null(timestamp))
##             warning("cannot sort without timestamp")
##         else  {
##             ot <- order(timestamp)
##             price <- price[ot]
##             amount <- amount[ot]
##             timestamp <- timestamp[ot]
##             instrument <- instrument[ot]
##         }
##     }
    
##     plfun <- function(amount, price) {
##         i <- amount > 0
##         if (abs(sum(amount)) > tol && do.warn) {
##             warning("sum of amount is not zero; cannot compute profit/loss.")
##             c(NA, sum(abs(amount)),
##               sum(price[ i] * amount[ i])/sum(amount[ i]),
##               sum(price[!i] * amount[!i])/sum(amount[!i]))
##         } else {
##             if (length(amount) > 1000L)
##                 p <- -drop(crossprod(amount, price)) else
##             p <- -sum(amount * price)
##             c(p, sum(abs(amount)),
##               sum(price[ i] * amount[ i])/sum(amount[ i]),
##               sum(price[!i] * amount[!i])/sum(amount[!i]))
##         }
##     }

##     if (!is.null(initial.position) && length(initial.position)) {
##         if (!is.null(names(initial.price)))
##             initial.price <- initial.price[match(attr(initial.position, "instrument"),
##                                                  names(initial.price))]        
##         amount0 <- c(initial.position)
##         instrument0 <- attr(initial.position, "instrument")
##         timestamp0 <- rep(attr(initial.position, "timestamp"), length(amount0))
##     } else {
##         initial.price <- timestamp0 <- amount0 <- numeric(0)
##         instrument0 <- character(0)
##     }
##     amount <- c(amount0, amount)
##     timestamp <- c(timestamp0, timestamp)
##     instrument <- c(instrument0, instrument)
##     price <- c(initial.price, price)
    
##     ## open position?
##     p1 <- position(amount, timestamp, instrument,
##                    drop.zero = TRUE)
##     if (length(p1) && !missing(current.price)) {
##         if (!is.null(names(current.price))) 
##             current.price <- current.price[match(attr(p1, "instrument"), names(current.price))]
##         amount1 <- c(-p1)  ## revert position
##         instrument1 <- attr(p1, "instrument")
##         timestamp1 <- attr(p1, "timestamp") 
##     } else {
##         current.price <- timestamp1 <- amount1 <- numeric(0)
##         instrument1 <- character(0)            
##     }
##     amount <- c(amount, amount1)
##     timestamp <- c(timestamp, timestamp1)
##     instrument <- c(instrument, instrument1)
##     price <- c(price, current.price)
    
##     if (!length(instrument))
##         instrument <- NULL    
##     ui <- unique(instrument)    
##     if (is.null(instrument) || length(ui) == 1L) {
##         if (along.timestamp) {
##             cumcash <- cumsum(-price * amount)
##             cumpos  <- cumsum(amount)
##             res <- list(value = cumpos * price + cumcash,
##                         position = cumpos,
##                         cash = cumcash + initial.cash)
##             class(res) <- "plsorted"
##         } else {
##             tmp <- plfun(amount, price)
##             res <- list(instrument   = if (is.null(instrument)) NA else ui,
##                         pl           = tmp[1L],
##                         ## subtract amounts that were not actually traded
##                         total.amount = tmp[2L] - sum(abs(amount0)) - sum(abs(amount1)),
##                         average.buy  = tmp[3L],
##                         average.sell = tmp[4L])
##             class(res) <- "pl"
##         }        
##     } else {
##         if (along.timestamp)
##             stop("currently only supported for a single instrument")            
##         instr <- sort(ui)
##         pls <- sumamounts <- mbuys <- msells <- numeric(length(instr))
##         for (i in seq_along(instr)) {
##             ix <- instr[i] == instrument  
##             tmp <- plfun(amount[ix], price[ix])
##             pls[i] <- tmp[1L]
##             sumamounts[i] <- tmp[2L]
##             mbuys[i] <- tmp[3L]
##             msells[i] <- tmp[4L]
##         }
##         res <- list(instrument = instr, 
##                     pl = pls,
##                     total.amount = sumamounts,
##                     average.buy = mbuys,
##                     average.sell = msells)
##         class(res) <- "pl"
##     }
##     res
## }

pl <- function(amount, ...)
    UseMethod("pl")

pl.journal <- function(amount, multiplier = 1,
                       along.timestamp = FALSE, approx = FALSE,
                       initial.position = NULL, initial.price = NULL,
                       eval.price = NULL,
                       tol = 1e-10, ...) {
    J <- amount
    price <- J$price
    instrument <- J$instrument
    amount <- J$amount
    timestamp <- J$timestamp
    pl.default(amount, price, timestamp,
               instrument,
               multiplier = multiplier,
               along.timestamp = along.timestamp,
               approx = approx,
               initial.position = initial.position, initial.price = initial.price,
               eval.price = eval.price,
               tol = tol, ...)
}

pl.default <- function(amount, price, timestamp = NULL,
                       instrument = NULL, multiplier = 1,
                       along.timestamp = FALSE,
                       approx = FALSE,
                       initial.position = NULL, initial.price = NULL,
                       eval.price = NULL,
                       tol = 1e-10, ...) {
    if (multiplier != 1)
        .NotYetUsed("multiplier")
    if (approx)
        .NotYetUsed("approx")
    if (is.null(instrument) || all(is.na(instrument))) {
        no.i <- TRUE
        instrument  <- rep("_", length(amount))
        uniq.i <- "_"
        ni <- 1L
        if (!is.null(eval.price))
            names(eval.price) <- "_"
        if (!is.null(initial.position))
            names(initial.position) <- "_"
        if (!is.null(initial.price))
            names(initial.price) <- "_"
    } else {
        no.i <- FALSE
        uniq.i <- sort(unique(instrument))
        ni <- length(uniq.i)
    }

    ans  <- vector(mode = "list", length = ni)
    for (i in seq_len(ni)) {
        i1 <- uniq.i[i]
        iv <- i1 == instrument & abs(amount) > tol
        amount1 <- amount[iv]
        price1 <- price[iv]
        if (!is.null(timestamp))
            timestamp1 <- timestamp[iv]
        eval.price1 <- eval.price[[ i1 ]]
        if (is.null(eval.price1))
            eval.price1 <- NA

        subtr <- 0
        if (!is.null(initial.position)) {
            ipos1 <- initial.position[[ i1 ]]
            iprice1 <- initial.price[[ i1 ]]

            subtr <- subtr + abs(ipos1)
            amount1 <- c(ipos1, amount1)
            price1 <- c(iprice1, price1)
        }

        open <- abs(sum(amount1)) > tol
        if (!open && !is.null(eval.price1) && !is.na(eval.price1)) {
            warning("all trades are closed ",
                    if (!no.i) paste0(" for ",  uniq.i[i]),
                    ", but ", sQuote("eval.price")," is specified")
        }

        if (open) {
            if (is.null(eval.price))
                warning(sQuote("sum(amount)"), " is not zero",
                        if (!no.i) paste0(" for ",  uniq.i[i]),
                        ": specify ",
                        sQuote("eval.price")," to compute p/l")
            subtr <- subtr + abs(sum(amount1))
            amount1 <- c(amount1, -sum(amount1))
            price1  <- c(price1, eval.price1)
        }

        pl1 <- .pl(amount1, price1, tol = tol, do.warn = FALSE)
        if (!along.timestamp) {
            tmp <- list(pl = pl1[1L],
                        realised = NA,
                        unrealised = NA,
                        buy = pl1[3L],
                        sell = pl1[4L],
                        volume = pl1[2L] - subtr)
        } else {
            cumcash <- cumsum(-price1 * amount1)
            cumpos  <- cumsum(amount1)
            pnl <- cumpos * price1 + cumcash
            real <- avg(amount1, price1)$realised
            tmp <- list(pl = pnl,
                        realised = real,
                        unrealised = pnl - real,
                        buy = pl1[3L],
                        sell = pl1[4L],
                        volume = cumsum(abs(amount1)))
        }
        ans[[i]] <- tmp
    }
    class(ans) <- "pl"
    attr(ans, "along.timestamp") <- along.timestamp
    if (no.i) {
        attr(ans, "instrument") <- NA
    } else {
        attr(ans, "instrument") <- uniq.i
        names(ans) <- uniq.i
    }
    ans
}

.pl <- function(amount, price, tol = 1e-10, do.warn = TRUE) {
    open <- abs(sum(amount)) > tol
    if (open && do.warn)
        warning(sQuote("sum(amount)"),
                " is not zero; cannot compute p/l.")
    i <- amount > 0
    c(if (open)
          NA
      else if (length(amount) > 1000L)
          -c(crossprod(amount, price))
      else
          -sum(amount * price),
      sum(abs(amount)),
      sum(price[ i] * amount[ i])/sum(amount[ i]),
      sum(price[!i] * amount[!i])/sum(amount[!i]))

}

as.data.frame.pl <- function(x, ...) {
    if (isTRUE(attr(x, "along.timestamp")))
        stop("currently only supported for ",
             sQuote("along.timestamp = FALSE"))

    ans <- data.frame(pl   = unlist(lapply(x, `[[`, "pl")),
                      buy  = unlist(lapply(x, `[[`, "buy")),
                      sell = unlist(lapply(x, `[[`, "sell")),
                      volume = unlist(lapply(x, `[[`, "volume")))
    if (!all(is.na(attr(x, "instrument"))))
        row.names(ans) <- attr(x, "instrument")
    ans
}


avg <- function(amount, price, tol = 1e-8) {
    if (any(rm <- abs(amount) < tol)) {
        warning("removed zero amounts")
        amount <-  amount[!rm]
        price  <- price[!rm]
    }
    cs <- cumsum(amount)
    acs <- abs(cs)
    av <- rd <- numeric(n <- length(cs))

    if (n == 1L) {
        list(average = price, realised = 0)
    } else {
        av[1L] <- price[1L]
        for (i in 2L:n) {
            i1 <- i-1L
            if (acs[i] > acs[i1]) {
                av[i] <- (av[i1] * cs[i1] + price[i]*amount[i])/
                    (amount[i] + cs[i1])
                rd[i] <- rd[i1]
            } else {
                av[i] <- av[i1]
                rd[i] <- rd[i1] + amount[i] * (av[i] - price[i])
            }
        }
        list(average = av, realised = rd)
    }
}
