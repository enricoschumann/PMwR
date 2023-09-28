## -*- truncate-lines: t; -*-
## Copyright (C) 2008-23  Enrico Schumann

pl <- function(amount, ...)
    UseMethod("pl")

print.pl <- function(x, ...,
                     use.crayon = NULL,
                     na.print = ".",
                     footnotes = TRUE) {
    if (is.null(use.crayon))
        use.crayon <- getOption("PMwR.use.crayon")

    if (is.null(use.crayon))
        use.crayon <- FALSE

    if (use.crayon && requireNamespace("crayon"))
        bold <- crayon::bold
    else
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

        no.trades <- length(x[[i]]$volume) == 1 &&
                            x[[i]]$volume  == 0L &&
                            x[[i]]$pl == 0L

        if (print.inst)
            cat(attr(x, "instrument")[[i]], "\n")

        PL   <- x[[i]]$pl
        BUY  <- x[[i]]$buy
        SELL <- x[[i]]$sell

        if (no.trades) {
            BUY <- SELL <- "."
            PL <- "0  <no trades>"
            w <- 1
        } else
            w <- max(0,
                     nchar(c(
                         as.character(prettyNum(c(PL, BUY, SELL,
                                                  x[[i]]$realised,
                                                  x[[i]]$unrealised))),
                         as.character(x[[i]]$timestamp))))

        if (!is.null(x[[i]]$timestamp) && length(x[[i]]$timestamp))
            cat(indent,      "timestamp     ",
                numrow(as.character(x[[i]]$timestamp), w),
                "\n", sep = "")
        cat(indent, bold("P/L total     "), bold(numrow(PL, w)) , "\n",
            if (any(!is.na(x[[i]]$realised)))
                paste0(indent, "__ realised   ",
                       numrow(x[[i]]$realised, w), "\n"),
            if (any(!is.na(x[[i]]$unrealised)))
                paste0(indent, "__ unrealised ",
                       numrow(x[[i]]$unrealised, w), "\n"),
            indent, "average buy   ", numrow(BUY, w), "\n",
            indent, "average sell  ", numrow(SELL, w), "\n",
            indent, "cum. volume   ",
            numrow(x[[i]]$volume, w), "\n", sep = "")

        if (i < ni)
            cat("\n")
    }

    if (footnotes) {
        cat("\n", sQuote("P/L total"), " is in units of instrument;\n",
            sQuote("volume"), " is sum of /absolute/ amounts.\n",
            sep = "")
        if (!is.null(fn <- attr(x, "footnotes"))) {
            i <- grep("average sell includes", fn)
            if (length(i) > 1L) {
                instr <- sub(".*For (.*): average sell includes.*", "\\1", fn[i])
                instr <- paste(instr, collapse = ", ")
                fn[i[1]] <- sub("(.*For )(.*)(: average sell includes.*)",
                                paste0("\\1", instr, "\\3"), fn[i[1]])
                fn <- fn[-i[-1]]
            }

            i <- grep("average buy includes", fn)
            if (length(i) > 1L) {
                instr <- sub(".*For (.*): average buy includes.*", "\\1", fn[i])
                instr <- paste(instr, collapse = ", ")
                fn[i[1]] <- sub("(.*For )(.*)(: average buy includes.*)",
                                paste0("\\1", instr, "\\3"), fn[i[1]])
                fn <- fn[-i[-1]]
            }

            message(paste(fn, collapse = "\n"))
        }
    }

    invisible(x)
}

pl.pl <- function(amount, ...) {
    ans <- unlist(lapply(amount, `[`, "pl"))
    names(ans) <- names(amount)
    ans
}

pl.journal <- function(amount, multiplier = 1,
                       multiplier.regexp = FALSE,
                       along.timestamp = FALSE, approx = FALSE,
                       initial.position = NULL, initial.price = NULL,
                       vprice = NULL,
                       tol = 1e-10, do.warn = TRUE,...) {

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
               tol = tol,
               do.warn = do.warn, ...)
}

pl.data.frame <- pl.journal

pl.default <- function(amount, price, timestamp = NULL,
                       instrument = NULL, multiplier = 1,
                       multiplier.regexp = FALSE,
                       along.timestamp = FALSE,
                       approx = FALSE,
                       initial.position = NULL,
                       initial.price = NULL,
                       vprice = NULL,
                       tol = 1e-10, do.warn = TRUE,
                       do.sum = FALSE, pl.only = FALSE,
                       footnotes = TRUE, ...) {

    if (length(multiplier) > 1L && is.null(names(multiplier)))
        stop(sQuote("multiplier"), " must be a named vector")
    if (approx)
        .NotYetUsed("approx")

    if (footnotes)
        fn <- NULL

    custom.timestamp <- FALSE
    if (isTRUE(along.timestamp)) {

        if (!length(timestamp) || all(is.na(timestamp)))
            timestamp <- seq_along(amount)
        else
            if (is.unsorted(timestamp)) {
                io <- order(timestamp)
                timestamp <- timestamp[io]
                amount <- amount[io]
                instrument <- instrument[io]
                price <- price[io]
            }

    } else if (!identical(along.timestamp, FALSE)) {

        ## User-defined timestamp: vprice needs to be
        ## specified and must be a matrix. In this
        ## case, along.timestamp is a vector of Dates,
        ## POSIXct, integers, ...

        custom.timestamp <- TRUE
        if (is.null(vprice))
            stop("user-defined timestamp: vprice must be specified")
        if (is.null(dim(vprice)))
            vprice <- as.matrix(vprice)

        if (is.null(timestamp) || !length(timestamp))
            stop(sQuote("along.timestamp"),
                 " specified but no timestamp available")

        if (nrow(vprice) != length(along.timestamp))
            stop("lengths of ", sQuote("vprice"), " and ",
                 sQuote("along.timestamp"), " differ")

        if (is.unsorted(along.timestamp)) {
            io <- order(along.timestamp)
            along.timestamp <- along.timestamp[io]
            vprice <- vprice[io, , drop = FALSE]
        }
    }

    ## empty amount
    if (!length(amount) && isTRUE(along.timestamp) &&
        is.null(initial.position)) {
        ans <- list(list(timestamp = numeric(0),
                         pl = numeric(0),
                         realised = numeric(0),
                         unrealised = numeric(0),
                         buy = numeric(0),
                         sell = numeric(0),
                         volume = 0))
        class(ans) <- "pl"
        attr(ans, "along.timestamp") <- along.timestamp
        attr(ans, "instrument") <- NA
        return(ans)
    }





    ## initial position should be a named vector
    if (!is.null(initial.position)) {

        if (inherits(initial.position, "journal")) {
            ## TODO: if price is specified, use as
            ##       initial.price => but only the latest
            ##       price should be used

            ## TODO: if timestamp is specified, use as
            ##       timestamp0 => but only the latest
            ##       timestamp should be used
            initial.position <- position(initial.position)
        }

        if (inherits(initial.position, "position"))
            initial.position <- vname(initial.position,
                                      attr(initial.position, "instrument"))

        if (do.warn &&
            any(abs(initial.position) > 0) &&
            is.null(initial.price))
            warning(sQuote("initial.position"),
                    " but no ",
                    sQuote("initial.price"))

        instrument0 <- names(initial.position)
        amount0 <- as.vector(initial.position)
        price0 <- if (!is.null(names(initial.position)))
                      initial.price[names(initial.position)]
                  else
                      initial.price

        instrument  <- c(instrument0, instrument)
        amount      <- c(amount0, amount)
        price       <- c(price0, price)
        ## TODO: is there a case for timestamp0 / initial.timestamp?

    }

    if (is.null(instrument) ||
        length(instrument) == 0L ||
        all(is.na(instrument))) {

        ## CASE 1: 'instrument' is missing, so a single
        ##         instrument is assumed

        no.i <- TRUE
        instrument  <- rep("_", length(amount))
        uniq.i <- "_"
        ni <- 1L
        if (!is.null(names(multiplier)) &&
            length(unique(names(multiplier))) > 1L &&
            length(multiplier) > 1L) {
            stop("named multipliers but instrument is not specified")
        }
        if (!is.null(vprice) &&
            !is.null(names(vprice)) &&
            length(unique(names(vprice))) > 1L &&
            length(vprice) > 1L) {
            stop("named vprice but instrument is not specified")
        }
        if (do.warn && !is.null(names(multiplier))) {
            warning("multiplier is named but instrument is not specified")
            multiplier <- unname(multiplier)
        }
        mult <- multiplier[1L]
        names(mult) <- "_"
        if (!is.null(vprice)) {
            if (custom.timestamp) {
                colnames(vprice) <- "_"
            } else {
                names(vprice) <- "_"
            }
        }

        if (!is.null(initial.position))
            instrument0 <- "_"

    } else {

        ## case 2: 'instrument' is supplied (even if it
        ##         is just a single unique instrument)

        no.i <- FALSE
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
                mult[] <- multiplier[1L]          ## are identical
            else
                mult <- multiplier[match(uniq.i, names(multiplier))]
        }
        ni <- length(uniq.i)
        if (ni == 1L && !is.null(vprice)) {
            if (custom.timestamp && is.null(colnames(vprice))) {
                colnames(vprice) <- uniq.i
            } else if (is.null(names(vprice))) {
                names(vprice) <- uniq.i
            }
        }
    }

    ans <- vector(mode = "list", length = ni)
    for (i in seq_len(ni)) {
        i1 <- uniq.i[i]
        iv <- i1 == instrument & abs(amount) > tol
        amount1 <- amount[iv]
        price1 <- price[iv]
        if (!is.null(timestamp) && length(timestamp) > 0L)
            timestamp1 <- timestamp[iv] else timestamp1 <- numeric(0)

        vprice1 <- NA
        if (!is.null(vprice)  && is.null(dim(vprice)))
            vprice1 <- vprice[ i1 ]
        if (!is.null(vprice) && !is.null(dim(vprice)))
            vprice1 <- vprice[ , i1]

        subtr <- if (!is.null(initial.position) && i1 %in% instrument0)
                     abs(amount0[i1 == instrument0])
                 else
                     0
        sum.amount1 <- sum(amount1)
        open <- abs(sum.amount1) > tol
        if (open && !custom.timestamp) {
            if (is.null(vprice) || is.na(vprice1)) {
                if (footnotes)
                    fn <- c(fn,
                            paste0(sQuote("sum(amount)"), " is not zero",
                                   if (!no.i) paste0(" for ",  uniq.i[i]),
                                   ": specify ",
                                   sQuote("vprice")," to compute P/L."))
                subtr <- 0
            } else {
                if (footnotes)
                    fn <- c(fn,
                            paste0(if (!no.i) paste0("For ",  uniq.i[i], ": "),
                                   if (sum.amount1 > 0) "average sell includes "
                                   else                 "average buy includes ",
                                   sQuote("vprice"), "."))

                subtr <- subtr + abs(sum.amount1)
                amount1 <- c(amount1, -sum.amount1)
                price1 <- c(price1, vprice1)
            }
        }

        pl1 <- .pl(amount1, price1, tol = tol, do.warn = FALSE)

        if (identical(along.timestamp, FALSE)) {

            ## total P/L, ignoring the timestamp

            tmp <- list(pl = pl1[1L] * unname(mult[i1]),
                        realised = NA,
                        unrealised = NA,
                        buy = pl1[3L],
                        sell = pl1[4L],
                        volume = pl1[2L] - subtr)

        } else {

            ## P/L along timestamp

            cumcash <- cumsum(-price1 * amount1)
            cumpos  <- cumsum(amount1)
            real <- if (length(amount1))
                        .pl_stats(amount1, price1)$realised else 0

            if (isTRUE(along.timestamp)) {

                ## use only timestamps in journal

                pnl <- cumpos * price1 + cumcash
                volume <- cumsum(abs(amount1))

            } else {

                ## compute position at every timestamp
                ## specified by 'along.timestamp',
                ## including position of cash account

                ## total pnl
                tmp <- position(amount = c(amount1, -price1 * amount1),
                                timestamp = c(timestamp1, timestamp1),
                                instrument = c(rep(i1, length(amount1)),
                                               rep("cash", length(amount1))),
                                when = along.timestamp)[, c(i1, "cash"), drop = FALSE]

                ## replace vprice1 with zero whenever there is no
                ## position in instrument: NA values are ignored
                vprice1[ abs(tmp[, i1])  < tol ] <- 0
                pnl <- rowSums(tmp * cbind(vprice1, 1))

                ## MATCH the elements in timestamp1 to
                ## along.timestamp. Several elements in
                ## timestamp1 may match a single
                ## timestamp in along.timestamp: Use
                ## the tail.
                matches <- matchOrNext(timestamp1, along.timestamp)
                real <- approx(unique(matches),
                               tapply(real, matches , tail, 1),
                               xout = seq_len(length(pnl)),
                               method = "constant", rule = 2,
                               yleft = 0, ties = "ordered")$y

                volume <- approx(unique(matches),
                                 tapply(cumsum(abs(amount1)),
                                        matches , tail, 1),
                                 xout = seq_len(length(pnl)),
                                 method = "constant", rule = 2,
                                 yleft = 0, ties = "ordered")$y

            }
            tmp <- list(timestamp = if (isTRUE(along.timestamp))
                                        timestamp1 else
                                        along.timestamp,
                        pl = pnl * unname(mult[i1]),
                        realised = real * unname(mult[i1]),
                        unrealised = pnl - real,
                        buy = pl1[3L],
                        sell = pl1[4L],
                        volume = volume)
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

    if (do.sum) {
        if ((n <- length(ans)) > 1L) {
            ans1 <- ans[[1L]]
            for (i in 2:n) {
                ans1$pl <- ans1$pl + ans[[i]]$pl
                ans1$realised <- ans1$realised + ans[[i]]$realised
                ans1$unrealised <- ans1$unrealised + ans[[i]]$unrealised
                ans1$volume <- ans1$volume + ans[[i]]$volume
            }
            ans1$buy <- NA
            ans1$sell <- NA
            ans1 <- list(ans1)
            class(ans1) <- "pl"
            attr(ans1, "along.timestamp") <- along.timestamp
            attr(ans1, "instrument") <- NA
            ans <- ans1
        }
    }
    if (pl.only) {
        if (!identical(along.timestamp, FALSE))
            stop("only supported for ",
                 sQuote("along.timestamp = FALSE"))
        ans1 <- unlist(lapply(ans, `[`, "pl"))
        names(ans1) <- names(ans)
        ans <- ans1
    }
    if (footnotes)
        attr(ans, "footnotes") <- fn
    ans
}

.pl <- function(amount, price, tol = 1e-10, do.warn = TRUE) {
    open <- abs(sum(amount)) > tol
    if (open && do.warn)
        warning(sQuote("sum(amount)"),
                " is not zero; cannot compute p/l")
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
    if (!identical(attr(x, "along.timestamp"), FALSE))
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

.pl_stats <- function(amount, price, tol = sqrt(.Machine$double.eps)) {
    cs <- cumsum(amount)
    acs <- abs(cs)
    av <- rd <- numeric(n <- length(cs))

    if (n == 1L) {
        list(average = price, realised = 0)
    } else {
        av[1L] <- price[1L]
        for (i in 2L:n) {
            i1 <- i - 1L
            if (sign(cs[i]) != sign(cs[i1])) {
                av[i] <- price[i]
                rd[i] <- rd[i1] + (price[i] - av[i1]) * cs[i1]
            } else if (acs[i] > acs[i1]) {
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

pl.btest <- function(amount, ...)
    pl(amount$journal, ...)
