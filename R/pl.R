print.pl <- function(x, ..., use.crayon = NULL, na.print = ".") {
    use.crayon <- if (is.null(use.crayon) &&
                      !is.null(tmp <- getOption("PMwR.use.crayon")))
                      tmp else FALSE
    if (!use.crayon)
        bold <- identity

    oo <- getOption("scipen")
    options(scipen = 1e8)
    on.exit(options(scipen = oo))

    ni <- length(x)

    numrow <- function(x, w) {
        ans <- substr(paste0(format(x, width = w, justify = "right"),
                             collapse = " "),
                      start = 1, stop = ceiling(getOption("width")*0.9))
        if (nchar(ans) < nchar(paste0(format(x, width = w), collapse = " ")))
            ans  <- paste(ans, "[...]")
        ans
    }

    indent <- ""
    print.inst <- FALSE
    if (!all(is.na(attr(x, "instrument")))) {
        print.inst <- TRUE
        indent <- "  "
    }

    for (i in seq_len(ni)) {
        if (print.inst)
            cat(attr(x, "instrument")[[i]], "\n")
        PL   <- if (is.finite(x[[i]]$pl)) ## TODO: what if pl has length>1?
                    x[[i]]$pl else na.print
        BUY  <- if (is.finite(x[[i]]$buy))
                    x[[i]]$buy else na.print
        SELL <- if (is.finite(x[[i]]$sell))
                    x[[i]]$sell else na.print
        w <- max(nchar(prettyNum(c(PL, BUY, SELL,
                                   x[[i]]$realised,
                                   x[[i]]$unrealised))))

        cat(indent, bold("P/L total     "), bold(numrow(PL, w)) , "\n",
            if (any(!is.na(x[[i]]$realised)))
                paste0(indent, "__ realised   ", numrow(x[[i]]$realised, w), "\n"),
            if (any(!is.na(x[[i]]$unrealised)))
                paste0(indent, "__ unrealised ", numrow(x[[i]]$unrealised, w), "\n"),
            indent, "average buy   ", numrow(BUY, w), "\n",
            indent, "average sell  ", numrow(SELL, w), "\n",
            indent, "volume        ",
            numrow(x[[i]]$volume, w), "\n", sep = "")

        if (i < ni)
            cat("\n")
    }
    cat("\n", sQuote("P/L total"), " is in units of instrument;\n",
        sQuote("volume"), " is sum of /absolute/ amounts.\n",
        sep = "")
    invisible(x)
}

pl <- function(amount, ...)
    UseMethod("pl")

pl.journal <- function(amount, multiplier = 1,
                       multiplier.regexp = FALSE,
                       along.timestamp = FALSE, approx = FALSE,
                       initial.position = NULL, initial.price = NULL,
                       vprice = NULL,
                       tol = 1e-10, ...) {
    J <- amount
    price <- J$price
    instrument <- J$instrument
    amount <- J$amount
    timestamp <- J$timestamp
    pl.default(amount, price, timestamp,
               instrument,
               multiplier = multiplier,
               multiplier.regexp = multiplier.regexp,
               along.timestamp = along.timestamp,
               approx = approx,
               initial.position = initial.position,
               initial.price = initial.price,
               vprice = vprice,
               tol = tol, ...)
}

pl.default <- function(amount, price, timestamp = NULL,
                       instrument = NULL, multiplier = 1,
                       multiplier.regexp = FALSE,
                       along.timestamp = FALSE,
                       approx = FALSE,
                       initial.position = NULL,
                       initial.price = NULL,
                       vprice = NULL,
                       tol = 1e-10, ...) {
    if (length(multiplier) > 1L && is.null(names(multiplier)))
        stop(sQuote("multiplier"), " must be a named vector")
    if (approx)
        .NotYetUsed("approx")

    ## initial position should be a named vector
    if (!is.null(initial.position)) {

        if (inherits(initial.position, "journal"))
            initial.position <- position(initial.position)
        ## TODO: if price is specified, use as initial.price
        ## TODO: if timestamp is specified, use as timestamp0
        
        if (inherits(initial.position, "position"))            
            initial.position <- vname(initial.position,
                                      attr(initial.position, "instrument"))
            
        if (any(abs(initial.position) > 0) &&
            is.null(initial.price))
            warning("initial.position but no initial.price")
        
        instrument0 <- names(initial.position)
        amount0 <- as.vector(initial.position)
        price0 <- if (!is.null(names(initial.position)))
                      initial.price[names(initial.position)]
                  else
                      initial.price

        instrument  <- c(instrument0, instrument)
        amount      <- c(amount0, amount)
        price       <- c(price0, price)
    }
    

    ## ipos.named <- !is.null(initial.position) &&
    ##     length(names(initial.position)) > 0L
    if (is.null(instrument) || length(instrument) == 0L ||
        all(is.na(instrument))) {
        no.i <- TRUE
        instrument  <- rep("_", length(amount))
        uniq.i <- "_"
        ni <- 1L
        mult <- multiplier[1]
        names(mult) <- "_"
        if (!is.null(vprice))
            names(vprice) <- "_"
        
        if (!is.null(initial.position)) { ## necessary to later
            instrument0 <- "_"            ## subtract i.pos from volume
        }
        ## if (!is.null(initial.price))
        ##     names(initial.price) <- "_"
    } else {
        no.i <- FALSE
        ## if (ipos.named)
        ##     instrument <- c(instrument, names(initial.position))
        uniq.i <- sort(unique(instrument))

        mult <- numeric(length(uniq.i))
        names(mult) <- uniq.i
        if (multiplier.regexp) {
            pattern <- names(multiplier)
            mult <- mult + NA
            for (i in seq_along(pattern)) {
                if (any(matched <- grepl(pattern[i], uniq.i, perl = TRUE))) {
                    mult[matched] <- multiplier[i]
                    names(mult)[matched] <- uniq.i[matched]
                }
            }
        } else {
            if (all(c(0, diff(multiplier)) == 0)) ## check if all multipliers
                mult[] <- multiplier[1]           ## are identical
            else
                mult <- multiplier[match(uniq.i, names(multiplier))]
        }
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

        vprice1  <- NA
        if (!is.null(vprice) && i1 %in% names(vprice))
            vprice1 <- vprice[[ i1 ]]
        ## else
        ##     vprice1  <- NA
        ## if (is.null(vprice1))
        ##     vprice1 <- NA

        subtr <- 0
        if (!is.null(initial.position) &&
            i1 %in% instrument0) {
            ## ipos1 <- initial.position[[ i1 ]]
            ipos1 <- amount0[i1 == instrument0]
            ## iprice1 <- initial.price[[ i1 ]]

            subtr <- subtr + abs(ipos1)
            ## amount1 <- c(ipos1, amount1)
            ## price1 <- c(iprice1, price1)
        }

        open <- abs(sum(amount1)) > tol
        if (!open && !is.null(vprice1) && !is.na(vprice1)) {
            warning("all trades are closed ",
                    if (!no.i) paste0(" for ",  uniq.i[i]),
                    ", but ", sQuote("vprice")," is specified")
        }

        if (open) {
            if (is.null(vprice))
                warning(sQuote("sum(amount)"), " is not zero",
                        if (!no.i) paste0(" for ",  uniq.i[i]),
                        ": specify ",
                        sQuote("vprice")," to compute p/l")
            subtr <- subtr + abs(sum(amount1))
            amount1 <- c(amount1, -sum(amount1))
            price1  <- c(price1, vprice1)
        }

        pl1 <- .pl(amount1, price1, tol = tol, do.warn = FALSE)
        if (!along.timestamp) {
            tmp <- list(pl = pl1[1L] * unname(mult[i1]),
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
            tmp <- list(pl = pnl * unname(mult[i1]),
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

pl.btest <- function(amount, ...) {
    pl(amount$journal)
}
