## -*- truncate-lines: t; -*-
## Copyright (C) 2008-19  Enrico Schumann

btest  <- function(prices,
                   signal,
                   do.signal = TRUE,
                   do.rebalance = TRUE,
                   print.info = NULL,
                   b = 1L,               ## burnin
                   fraction = 1,         ## how much to rebalance
                   initial.position = 0,
                   initial.cash = 0,
                   final.position = FALSE,
                   cashflow = NULL,
                   tc = 0,
                   ...,
                   add = FALSE,          ## if TRUE, 'position' is flow
                   lag = 1,
                   convert.weights = FALSE,
                   trade.at.open = TRUE,
                   tol = 1e-5,
                   tol.p = NA,
                   Globals = list(),
                   prices0 = NULL,
                   include.data = FALSE,
                   include.timestamp = TRUE,
                   timestamp, instrument,
                   progressBar = FALSE,
                   variations,
                   variations.settings = list(),
                   replications) {

    if (!missing(variations)) {
        x <- match.call()
        all_args <- as.list(x)[-1L]
        all_args <- lapply(all_args, eval)
        variations <- all_args$variations
        all_args$variations <- NULL

        vsettings <- list(method = "loop",
                          load.balancing = FALSE,
                          cores = getOption("mc.cores", 2L))
        vsettings[names(variations.settings)] <- variations.settings
        all_args$variations.settings <- NULL

        lens <- lengths(variations)
        cases <- do.call(expand.grid,
                         lapply(lens, seq_len))
        args <- vector("list", length = nrow(cases))

        for (i in seq_along(args)) {
            tmp <- mapply(`[[`, variations, cases[i, ],
                          SIMPLIFY = FALSE)
            args[[i]] <- c(all_args, tmp)
            attr(args[[i]], "variation") <- tmp
        }
        if (is.null(vsettings$method) ||
            vsettings$method == "loop") {
            ans <- vector("list", length(args))
            for (i in seq_along(args)) {
                ans[[i]] <- do.call(btest, args[[i]])
                attr(ans[[i]], "variation") <- attr(args[[i]], "variation")
            }
            if (!is.null(vsettings$label))
                names(ans) <- vsettings$label
            return(ans)

        } else if (vsettings$method == "parallel" ||
                   vsettings$method == "snow") {
            if (!requireNamespace("parallel"))
                stop("package ", sQuote("parallel"), " not available")
            if (is.null(vsettings$cl) && is.numeric(vsettings$cores))
                cl <- parallel::makeCluster(c(rep("localhost", vsettings$cores)),
                                            type = "SOCK")
            on.exit(parallel::stopCluster(cl))
            ans <- parallel::parLapplyLB(cl, X = args,
                                         fun = function(x) do.call("btest", x))
            if (!is.null(vsettings$label))
                names(ans) <- vsettings$label
            return(ans)

        } else if (vsettings$method == "multicore") {
            if (!requireNamespace("parallel"))
                stop("package ", sQuote("parallel"), " not available")
            ans <- parallel::mclapply(X = args,
                                      FUN = function(x) do.call("btest", x),
                                      mc.cores = vsettings$cores)
            if (!is.null(vsettings$label))
                names(ans) <- vsettings$label
            return(ans)
        }
    } else if (!missing(replications)) {
        x <- match.call()
        all_args <- as.list(x)[-1L]
        all_args <- lapply(all_args, eval)
        replications <- all_args$replications
        all_args$replications <- NULL

        vsettings <- list(method = "loop",
                          load.balancing = FALSE,
                          cores = getOption("mc.cores", 2L))
        vsettings[names(variations.settings)] <- variations.settings
        all_args$variations.settings <- NULL

        if (is.null(vsettings$method) ||
            vsettings$method == "loop") {
            ans <- vector("list", replications)
            for (i in seq_len(replications)) {
                ans[[i]] <- do.call(btest, all_args)
                attr(ans[[i]], "replication") <- i
            }
            if (!is.null(vsettings$label))
                names(ans) <- vsettings$label
            return(ans)

        } else if (vsettings$method == "parallel" ||
                   vsettings$method == "snow") {
            if (!requireNamespace("parallel"))
                stop("package ", sQuote("parallel"), " not available")
            if (is.null(vsettings$cl) && is.numeric(vsettings$cores))
                cl <- parallel::makeCluster(
                                    c(rep("localhost", vsettings$cores)),
                                    type = "SOCK")
            on.exit(parallel::stopCluster(cl))
            clusterExport(cl, "all_args", environment())
            if (vsettings$load.balancing)
                ans <- parallel::parLapplyLB(cl, X = seq_len(replications),
                                             fun = function(i) {
                                                 ans <- do.call("btest", all_args)
                                                 attr(ans, "replication") <- i
                                                 ans})
            else
                ans <- parallel::parLapply(cl, X = seq_len(replications),
                                           fun = function(i) {
                                               ans <- do.call("btest", all_args)
                                               attr(ans, "replication") <- i
                                               ans})

            if (!is.null(vsettings$label))
                names(ans) <- vsettings$label
            return(ans)

        } else if (vsettings$method == "multicore") {
            if (!requireNamespace("parallel"))
                stop("package ", sQuote("parallel"), " not available")
            ans <- parallel::mclapply(X = seq_len(replications),
                                      FUN = function(i) {
                                          ans <- do.call("btest", all_args)
                                             attr(ans, "replication") <- i
                                             ans
                                      },
                                      mc.cores = vsettings$cores)
            if (!is.null(vsettings$label))
                names(ans) <- vsettings$label
            return(ans)
        }
    }

    L <- lag

    tc_fun <- if (is.function(tc))
                  tc

    if (!missing(timestamp) &&
        (inherits(timestamp, "Date") || inherits(timestamp, "POSIXct")) &&
        inherits(b, class(timestamp))) {
        b <- if (b < min(timestamp))
                 0
             else
                 .match_or_previous(b, timestamp)
    }

    if ("tradeOnOpen" %in% names(list(...)))
        warning("Did you mean 'trade.at.open'? See ChangeLog 2017-11-14.")

    if ("assignInGlobals" %in% names(list(...)))
        warning("Did you mean 'Globals'? See ChangeLog 2017-11-14.")

    if (convert.weights && initial.cash == 0)
        warning(sQuote("convert.weights"), " is TRUE and ",
                sQuote("initial.cash"), " is zero")

    if (convert.weights && b == 0 && is.null(prices0) && lag > 0)
        stop("to convert weights to positions, either specify ",
                sQuote("prices0"), " or set ", sQuote("b"), " > 0")

    if (add)
        .NotYetUsed("add", FALSE)

    db.signal <- if (is.function(signal) && isdebugged(signal))
        TRUE else FALSE

    if (is.function(do.signal) && isdebugged(do.signal))
        db.do.signal <- TRUE
    else
        db.do.signal <- FALSE

    if (is.function(do.rebalance) && isdebugged(do.rebalance))
        db.do.rebalance <- TRUE
    else
        db.do.rebalance <- FALSE

    if (is.function(print.info) && isdebugged(print.info))
        db.print.info <- TRUE
    else
        db.print.info <- FALSE

    db.cashflow <- if (is.function(cashflow) && isdebugged(cashflow))
                       TRUE else FALSE

    db.tc_fun <- if (is.function(tc) && isdebugged(tc))
                       TRUE else FALSE

    if (is.null(do.signal) || identical(do.signal, TRUE)) {
        do.signal <- function(...)
            TRUE
    } else if (identical(do.signal, FALSE) && !final.position) {
        do.signal <- function(...)
            FALSE
        warning(sQuote("do.signal"), " is FALSE: strategy will never trade")
    } else if (!missing(timestamp) && inherits(do.signal, class(timestamp))) {
        rebalancing_times <- matchOrNext(do.signal, timestamp)
        do.signal <- function(...)
            Time(0L) %in% rebalancing_times
    } else if (is.numeric(do.signal)) {
        ## TODO: what if Date?
        rebalancing_times <- do.signal
        do.signal <- function(...) {
            if (Time(0L) %in% rebalancing_times)
                TRUE
            else
                FALSE
        }
    } else if (is.logical(do.signal)) {
        ## tests on identical to TRUE,FALSE above, so length > 1
        rebalancing_times <- which(do.signal)
        do.signal <- function(...)
            if (Time(0L) %in% rebalancing_times)
                TRUE
            else
                FALSE
    }  else if (is.character(do.signal) &&
               tolower(do.signal) == "firstofmonth") {
        tmp <- as.Date(timestamp)
        if (any(is.na(tmp)))
            stop("timestamp with NAs")
        ii <- if (b > 0)
                  -seq_len(b)
              else
                  TRUE
        i_rdays <- match(aggregate(tmp[ii],
                                   by = list(format(tmp[ii], "%Y-%m")),
                                   FUN = head, 1)[[2L]],
                         tmp)
        do.signal <- function(...)
            Time(0) %in% i_rdays
    } else if (is.character(do.signal) &&
               (tolower(do.signal) == "lastofmonth" ||
                tolower(do.signal) == "endofmonth")) {
        tmp <- as.Date(timestamp)
        if (any(is.na(tmp)))
            stop("timestamp with NAs")
        ii <- if (b > 0)
                  -seq_len(b)
              else
                  TRUE
        i_rdays <- match(aggregate(tmp[ii],
                                   by = list(format(tmp[ii], "%Y-%m")),
                                   FUN = tail, 1)[[2L]],
                         tmp)
        do.signal <- function(...)
            Time(0) %in% i_rdays
    } else if (is.character(do.signal) &&
               tolower(do.signal) == "firstofquarter") {
        tmp <- as.Date(timestamp)
        if (any(is.na(tmp)))
            stop("timestamp with NAs")
        ii <- if (b > 0)
                  -seq_len(b)
              else
                  TRUE
        i_rdays <- match(aggregate(tmp[ii],
                                   by = list(paste0(format(tmp[ii], "%Y"), "-", quarters(tmp[ii]))),
                                   FUN = head, 1)[[2L]],
                         tmp)
        do.signal <- function(...)
            Time(0) %in% i_rdays
    } else if (is.character(do.signal) &&
               tolower(do.signal) == "lastofquarter") {
        tmp <- as.Date(timestamp)
        if (any(is.na(tmp)))
            stop("timestamp with NAs")
        ii <- if (b > 0)
                  -seq_len(b)
              else
                  TRUE
        i_rdays <- match(aggregate(tmp[ii],
                                   by = list(paste0(format(tmp[ii], "%Y"), "-", quarters(tmp[ii]))),
                                   FUN = tail, 1)[[2L]],
                         tmp)
        do.signal <- function(...)
            Time(0) %in% i_rdays
    }




    if (is.null(do.rebalance) || identical(do.rebalance, TRUE)) {
        do.rebalance <- function(...)
            TRUE
    } else if (identical(do.rebalance, FALSE)) {
        do.rebalance <- function(...)
            FALSE
        warning(sQuote("do.rebalance"), " is FALSE: strategy will never trade")
    } else if (identical(do.rebalance, "do.signal")) {
        do.rebalance <- function(...)
            computeSignal
    } else if (!missing(timestamp) && inherits(do.rebalance, class(timestamp))) {
        rebalancing_times <- matchOrNext(do.rebalance, timestamp)
        do.rebalance <- function(...)
            Time(0L) %in% rebalancing_times
    } else if (is.numeric(do.rebalance)) {
        rebalancing_times <- do.rebalance
        do.rebalance <- function(...) {
            Time(0L) %in% rebalancing_times
        }
    } else if (is.logical(do.rebalance)) {
        ## tests on identical to TRUE,FALSE above, so length > 1
        rebalancing_times <- which(do.rebalance)
        do.rebalance <- function(...)
            if (Time(0L) %in% rebalancing_times)
                TRUE
            else
                FALSE
    }  else if (is.character(do.rebalance) &&
               tolower(do.rebalance) == "firstofmonth") {
        tmp <- as.Date(timestamp)
        if (any(is.na(tmp)))
            stop("timestamp with NAs")
        ii <- if (b > 0)
                  -seq_len(b)
              else
                  TRUE
        i_rdays <- match(aggregate(tmp[ii],
                                   by = list(format(tmp[ii], "%Y-%m")),
                                   FUN = head, 1)[[2L]],
                         tmp)
        do.rebalance <- function(...)
            Time(0) %in% i_rdays
    } else if (is.character(do.rebalance) &&
               (tolower(do.rebalance) == "lastofmonth" ||
                tolower(do.rebalance) == "endofmonth")) {
        tmp <- as.Date(timestamp)
        if (any(is.na(tmp)))
            stop("timestamp with NAs")
        ii <- if (b > 0)
                  -seq_len(b)
              else
                  TRUE
        i_rdays <- match(aggregate(tmp[ii],
                                   by = list(format(tmp[ii], "%Y-%m")),
                                   FUN = tail, 1)[[2L]],
                         tmp)
        do.rebalance <- function(...)
            Time(0) %in% i_rdays
    } else if (is.character(do.rebalance) &&
               tolower(do.rebalance) == "firstofquarter") {
        tmp <- as.Date(timestamp)
        if (any(is.na(tmp)))
            stop("timestamp with NAs")
        ii <- if (b > 0)
                  -seq_len(b)
              else
                  TRUE
        i_rdays <- match(aggregate(tmp[ii],
                                   by = list(list(paste0(format(tmp[ii], "%Y"), "-", quarters(tmp[ii])))),
                                   FUN = head, 1)[[2L]],
                         tmp)
        do.rebalance <- function(...)
            Time(0) %in% i_rdays
    } else if (is.character(do.rebalance) &&
               tolower(do.rebalance) == "lastofquarter") {
        tmp <- as.Date(timestamp)
        if (any(is.na(tmp)))
            stop("timestamp with NAs")
        ii <- if (b > 0)
                  -seq_len(b)
              else
                  TRUE
        i_rdays <- match(aggregate(tmp[ii],
                                   by = list(list(paste0(format(tmp[ii], "%Y"), "-", quarters(tmp[ii])))),
                                   FUN = tail, 1)[[2L]],
                         tmp)
        do.rebalance <- function(...)
            Time(0) %in% i_rdays
    }

    ## if (is.null(do.rebalance) || identical(do.rebalance, TRUE)) {
    ##     do.rebalance <- function(...)
    ##         TRUE
    ## } else if (identical(do.rebalance, FALSE)) {
    ##     do.rebalance <- function(...)
    ##         FALSE
    ## }

    if (is.null(cashflow)) {
        cashflow <- function(...)
            0
    } else if (is.numeric(cashflow)) {
        cashflow <- function(...)
            cashflow[1L]
    }

    doPrintInfo <- TRUE
    if (is.null(print.info)) {
        doPrintInfo <- FALSE
        print.info <- function(...)
            NULL
    }

    ## functions available in within functions such as 'signal'
    Open <- function(lag = L, n = NA) {
        if (!is.na(n))
            mO[t - (n:1), , drop = FALSE]
        else
            mO[t - lag, , drop = FALSE]
    }
    High <- function(lag = L, n = NA) {
        if (!is.na(n))
            mH[t - (n:1), , drop = FALSE]
        else
            mH[t - lag, , drop = FALSE]
    }
    Low <- function(lag = L, n = NA) {
        if (!is.na(n))
            mL[t - (n:1), , drop = FALSE]
        else
            mL[t - lag, , drop = FALSE]
    }
    Close <- function(lag = L, n = NA) {
        if (!is.na(n))
            mC[t - (n:1), , drop = FALSE]
        else
            mC[t - lag, , drop = FALSE]
    }
    Wealth <- function(lag = L, n = NA) {
        if (!is.na(n))
            v[t - (n:1)]
        else
            v[t - lag]
    }
    Cash <- function(lag = L, n = NA) {
        if (!is.na(n))
            cash[t - (n:1)]
        else
            cash[t - lag]
    }
    Time <- function(lag = L, n = NA) {
        if (!is.na(n))
            t - (n:1)
        else
            t - lag
    }
    Portfolio <- function(lag = L, n = NA) {
        if (!is.na(n))
            X[t - (n:1), , drop = FALSE]
        else
            X[t - lag, , drop = FALSE]
    }
    SuggestedPortfolio <- function(lag = L, n = NA) {
        if (!is.na(n))
            Xs[t - (n:1), , drop = FALSE]
        else
            Xs[t - lag, , drop = FALSE]
    }

    if (!missing(timestamp)) {
        Timestamp <- function(lag = L, n = NA) {
            if (!is.na(n))
                timestamp[t - (n:1)]
            else
                timestamp[t - lag]
        }
    } else
        Timestamp <- Time

    ## create Globals
    Globals <- list2env(Globals)

    ## check reserved names
    reservedNames <- c("Open", "High", "Low", "Close",
                       "Wealth", "Cash", "Time", "Timestamp",
                       "Portfolio", "SuggestedPortfolio", "Globals")
    funs <- c("signal", "do.signal", "do.rebalance", "print.info", "cashflow")
    if (!is.null(tc_fun))
        funs <- c(funs, "tc_fun")
    for (thisfun in funs) {
        fNames <- names(formals(get(thisfun)))
        for (rname in reservedNames)
            if (rname %in% fNames)
                stop(sQuote(rname), " cannot be used as an argument name for ",
                     sQuote(thisfun))}

    add.args <- alist(Open = Open,
                      High = High,
                      Low = Low,
                      Close = Close,
                      Wealth = Wealth,
                      Cash = Cash,
                      Time = Time,
                      Timestamp = Timestamp,
                      Portfolio = Portfolio,
                      SuggestedPortfolio = SuggestedPortfolio,
                      Globals = Globals)
    formals(signal) <- c(formals(signal), add.args)
    if (db.signal)
        debug(signal)

    formals(do.signal) <- c(formals(do.signal), add.args)
    if (db.do.signal)
        debug(do.signal)

    formals(do.rebalance) <- c(formals(do.rebalance), add.args)
    if (db.do.rebalance)
        debug(do.rebalance)

    formals(print.info) <- c(formals(print.info), add.args)
    if (db.print.info)
        debug(print.info)

    formals(cashflow) <- c(formals(cashflow), add.args)
    if (db.cashflow)
        debug(cashflow)

    if (!is.null(tc_fun)) {
        formals(tc_fun) <- c(formals(tc_fun), add.args)
        if (db.tc_fun)
            debug(tc_fun)
    }

    if (is.list(prices)) {
        if (length(prices) == 1L) {

            mC <- prices[[1L]]
            if (is.null(dim(mC)))
                mC <- as.matrix(mC)
            trade.at.open <- FALSE

        } else if (length(prices) == 4L) {
            mO <- prices[[1L]]
            mH <- prices[[2L]]
            mL <- prices[[3L]]
            mC <- prices[[4L]]

            if (is.null(dim(mO)))
                mO <- as.matrix(mO)
            if (is.null(dim(mH)))
                mH <- as.matrix(mH)
            if (is.null(dim(mL)))
                mL <- as.matrix(mL)
            if (is.null(dim(mC)))
                mC <- as.matrix(mC)
        } else
            stop("see documentation on ", sQuote("prices"))

    } else {

        if (is.null(dim(prices)))
            prices <- as.matrix(prices)

        if (ncol(prices) == 1L) {
            mC <- prices
            trade.at.open <- FALSE
        } else if (ncol(prices) == 4L) {
            mO <- prices[, 1L]
            mH <- prices[, 2L]
            mL <- prices[, 3L]
            mC <- prices[, 4L]
        } else
            stop("see documentation on ", sQuote("prices"))
    }

    ## param .... settings
    T <- nrow(mC)
    nA <- ncol(mC)

    if (!missing(timestamp) && length(timestamp) != T)
        warning("length(timestamp) does not match nrow(prices)")
    if (!missing(instrument) && length(instrument) != nA)
        warning("length(instrument) does not match ncol(prices)")

    ## tc can be of length nA or length 1L
    tccum <- numeric(T)

    ## X  .... actual portfolios over time
    ## Xs .... signals (recommended)
    X  <- array(NA, dim = c(T, nA))
    Xs <- array( 0, dim = c(T, nA))
    colnames(X) <- colnames(mC)
    colnames(Xs) <- colnames(mC)
    v <- cash <- numeric(T)
    v[] <- NA
    if (b > 0L) {
        Xs[b, ] <- X[b, ] <- initial.position
        cash[b] <- initial.cash
        v[b] <- initial.cash + if (initial.position != 0)
                                   initial.position %*% mC[b, ] else 0
    }

    ## initial wealth
    if (initial.position != 0 && !is.null(prices0)) {
        initial.wealth <- sum(prices0 * initial.position) + initial.cash
    } else if (initial.position != 0) {
        warning(sQuote("initial.position"), " specified, but no ", sQuote("prices0"))
        initial.wealth <- initial.cash ## TODO: initial position needs be evaluated
    } else
        initial.wealth <- initial.cash

    ## period 1: code is only used if  b == 0L
    if (b == 0L) {
        t <- 1L
        computeSignal <- do.signal(...,
                                   Open = Open, High = High,
                                   Low = Low, Close = Close,
                                   Wealth = Wealth, Cash = Cash,
                                   Time = Time, Timestamp = Timestamp,
                                   Portfolio = Portfolio,
                                   SuggestedPortfolio = SuggestedPortfolio,
                                   Globals = Globals)

        if (computeSignal) {
            temp <- signal(..., Open = Open, High = High, Low = Low,
                           Close = Close, Wealth = Wealth, Cash = Cash,
                           Time = Time, Timestamp = Timestamp,
                           Portfolio = Portfolio,
                           SuggestedPortfolio = SuggestedPortfolio,
                           Globals = Globals)

            if (!is.null(temp)) {
                if (convert.weights) {
                    temp0 <- temp != 0
                    temp[temp0] <- temp[temp0] *
                                   initial.wealth/prices0[temp0]
                }
                Xs[t, ] <- temp
            } else
                Xs[t, ] <- 0
            computeSignal <- FALSE
        } else {
            Xs[t, ] <- rep.int(0, nA)
        }

        ## REBALANCE?
        rebalance <- do.rebalance(...,
                                  Open = Open, High = High,
                                  Low = Low, Close = Close,
                                  Wealth = Wealth, Cash = Cash,
                                  Time = Time, Timestamp = Timestamp,
                                  Portfolio = Portfolio,
                                  SuggestedPortfolio = SuggestedPortfolio,
                                  Globals = Globals)

        dXs <- Xs[t, ] - if (any(initial.position != 0))
                             initial.position else 0

        if (max(abs(dXs)) < tol)
            rebalance <- FALSE

        if (rebalance) {

            if (!is.null(tc_fun))
                tc <- tc_fun(...,
                             Open = Open, High = High,
                             Low = Low, Close = Close,
                             Wealth = Wealth, Cash = Cash,
                             Time = Time, Timestamp = Timestamp,
                             Portfolio = Portfolio,
                             SuggestedPortfolio = SuggestedPortfolio,
                             Globals = Globals)

            dx <- fraction * dXs

            if (trade.at.open) ## will convert m(O|C) to vector
                open <- mO[t, , drop = TRUE]
            else
                open <- mC[t, , drop = TRUE]
            sx <- dx %*% open
            abs_sx <- (abs(dx) * tc) %*% open
            tccum[t] <- abs_sx
            cash[t] <- initial.cash - sx - abs_sx
            X[t, ] <- if (any(initial.position != 0)) initial.position else 0  + dx
            rebalance <- FALSE
        } else {
            tccum[t] <- 0
            cash[t] <- initial.cash
            X[t, ] <- ifelse(initial.position != 0, initial.position, 0)
        }

        ## cashflow
        cash[t] <- cash[t] +
                   cashflow(...,
                            Open = Open,
                            High = High,
                            Low = Low,
                            Close = Close,
                            Wealth = Wealth,
                            Cash = Cash,
                            Time = Time,
                            Timestamp = Timestamp,
                            Portfolio = Portfolio,
                            SuggestedPortfolio = SuggestedPortfolio,
                            Globals = Globals)

        v[t] <- X[t, ] %*% mC[t, ] + cash[t]
        if (doPrintInfo)
            print.info(..., Open = Open, High = High, Low = Low,
                       Close = Close, Wealth = Wealth, Cash = Cash,
                       Time = Time, Timestamp = Timestamp,
                       Portfolio = Portfolio,
                       SuggestedPortfolio = SuggestedPortfolio,
                       Globals = Globals)
    }
    ## end period 1

    if (progressBar)
        progr <- txtProgressBar(min = max(2L, b+1L), max = T,
                                initial = max(2L, b+1L),
                                char = if (.Platform$OS.type == "unix") "\u2588" else "|",
                                width = ceiling(getOption("width")*0.8),
                                style = 3, file = "")

    for (t in max(2L, b+1L):T) {
        if (progressBar)
            setTxtProgressBar(progr, t)
        t1 <- t - 1L
        computeSignal <- do.signal(...,
                                   Open = Open, High = High,
                                   Low = Low, Close = Close,
                                   Wealth = Wealth,
                                   Cash = Cash,
                                   Time = Time,
                                   Timestamp = Timestamp,
                                   Portfolio = Portfolio,
                                   SuggestedPortfolio = SuggestedPortfolio,
                                   Globals = Globals)

        if (computeSignal) {
            temp <- signal(..., Open = Open, High = High,
                           Low = Low, Close = Close, Wealth = Wealth,
                           Cash = Cash, Time = Time,
                           Timestamp = Timestamp,
                           Portfolio = Portfolio,
                           SuggestedPortfolio = SuggestedPortfolio,
                           Globals = Globals)

            if (!is.null(temp)) {
                if (convert.weights) {
                    temp0 <- temp != 0
                    temp[temp0] <- temp[temp0] * v[t1] / mC[t1, temp0]
                }
                Xs[t, ] <- temp
            } else
                Xs[t, ] <- Xs[t1, ] ## b0

            computeSignal <- FALSE
        } else {
            Xs[t, ] <- Xs[t1, ] ## b0
        }


        rebalance <- do.rebalance(..., Open = Open, High = High,
                                  Low = Low, Close = Close,
                                  Wealth = Wealth, Cash = Cash,
                                  Time = Time, Timestamp = Timestamp,
                                  Portfolio = Portfolio,
                                  SuggestedPortfolio = SuggestedPortfolio,
                                  Globals = Globals)

        dXs <- Xs[t, ] - X[t1, ]  ## b0
        if (max(abs(dXs)) < tol)
            rebalance <- FALSE
        else if (!is.na(tol.p) && initial.wealth > 0 && v[t1] > 0 ) {
            dXs.p <- dXs * mC[t1, ]/v[t1]
            small <- abs(dXs.p) < tol.p
            dXs[small] <- 0
            if (all(small))
                rebalance  <- FALSE
        }

        if (rebalance) {
            dx <- fraction * dXs

            if (!is.null(tc_fun))
                tc <- tc_fun(...,
                             Open = Open, High = High,
                             Low = Low, Close = Close,
                             Wealth = Wealth, Cash = Cash,
                             Time = Time, Timestamp = Timestamp,
                             Portfolio = Portfolio,
                             SuggestedPortfolio = SuggestedPortfolio,
                             Globals = Globals)

            if (trade.at.open) ## will convert m(O|C) to vector (drop == TRUE)
                open <- mO[t, ]
            else
                open <- mC[t, ]

            nzero <- dx != 0
            sx <- dx[nzero] %*% open[nzero]
            abs_sx <- (abs(dx[nzero]) * tc) %*% open[nzero]
            tccum[t] <- tccum[t1] + abs_sx
            cash[t] <- cash[t1] - sx - abs_sx
            X[t, ] <- X[t1, ] + dx
            rebalance <- FALSE
        } else {
            tccum[t] <- tccum[t1]
            cash[t] <- cash[t1]
            X[t, ] <- X[t1, ]
        }

        ## cashflow
        cash[t] <- cash[t] + cashflow(...,
                                      Open = Open, High = High,
                                      Low = Low, Close = Close,
                                      Wealth = Wealth,
                                      Cash = Cash,
                                      Time = Time, Timestamp = Timestamp,
                                      Portfolio = Portfolio,
                                      SuggestedPortfolio = SuggestedPortfolio,
                                      Globals = Globals)


        ## WEALTH
        nzero <- X[t, ] != 0
        v[t] <- X[t, nzero] %*% mC[t, nzero] + cash[t]

        if (doPrintInfo)
            print.info(..., Open = Open,
                            High = High,
                            Low = Low,
                            Close = Close,
                            Wealth = Wealth,
                            Cash = Cash,
                            Time = Time, Timestamp = Timestamp,
                            Portfolio = Portfolio,
                            SuggestedPortfolio = SuggestedPortfolio,
                            Globals = Globals)
    } ## end of for loop

    if (progressBar)
        close(progr)

    if (final.position) {
        message("Compute final position ... ", appendLF = FALSE)
        t  <- t + 1L
        t1 <- t - 1L
        final.pos <- signal(..., Open = Open, High = High,
                            Low = Low, Close = Close, Wealth = Wealth,
                            Cash = Cash, Time = Time,
                            Timestamp = Timestamp,
                            Portfolio = Portfolio,
                            SuggestedPortfolio = SuggestedPortfolio,
                            Globals = Globals)
        if (convert.weights)
            final.pos <- final.pos * v[t1] / mC[t1, ]
        if (!missing(instrument))
            names(final.pos) <- instrument
        message("done")
    }

    if (!missing(instrument))
        colnames(Xs) <- colnames(X) <- instrument
    if (is.null(colnames(X)))
        colnames(Xs) <- colnames(X) <- paste("asset", seq_len(ncol(X)))
    if (missing(timestamp))
        timestamp <- seq_len(nrow(X))


    ##  ------------- [[ journal ]] -------------
    ## TODO include cash in journal
    trades <- diff(rbind(initial.position, X))
    keep <- abs(trades) > sqrt(.Machine$double.eps) & !is.na(trades)
    if (any(keep)) {
        j.timestamp <- list()
        j.amount <- list()
        j.price <- list()
        j.instrument <- list()
        for (cc in seq_len(ncol(X))) {
            ic <- keep[, cc]
            if (!any(ic))
                next
            ccc <- as.character(cc)
            j.timestamp[[ccc]] <- timestamp[ic]
            j.amount[[ccc]] <- trades[ic, cc]
            j.price[[ccc]] <- mC[ic, cc]
            j.instrument[[ccc]] <- rep(colnames(X)[cc], sum(ic))
        }
        j.timestamp <- do.call(c, j.timestamp)
        j.amount <- do.call(c, j.amount)
        j.price <- do.call(c, j.price)
        j.instrument <- do.call(c, j.instrument)
        jnl <- journal(timestamp  = unname(j.timestamp),
                       amount     = unname(j.amount),
                       price      = unname(j.price),
                       instrument = unname(j.instrument))
        jnl <- sort(jnl)
    } else
        jnl <- journal()


    ans <- list(position = X,
                suggested.position = Xs,
                cash = cash,
                wealth = v,
                cum.tc = tccum,
                journal = jnl,
                initial.wealth = initial.wealth,
                b = b,
                final.position = if (final.position) final.pos else NA,
                Globals = Globals)

    if (include.timestamp)
        ans <- c(ans,
                 timestamp = list(timestamp))

    if (include.data)
        ans <- c(ans,
                 prices = prices,
                 signal = signal,
                 do.signal = do.signal,
                 instrument = if (missing(instrument)) NULL else list(instrument),
                 call = match.call())

    class(ans) <- "btest"
    ans
}

print.btest <- function(x, ...) {
    ## TODO: check if timestamp exists and can be coerced to Date
    cat("initial wealth",
        tmp0 <- x$wealth[min(which(!is.na(x$wealth)))], " =>  ")
    cat("final wealth ",
        tmp1 <- round(x$wealth[max(which(!is.na(x$wealth)))], 2), "\n")
    if (tmp0 > 0)
        cat("Total return   ", round(100*(tmp1/tmp0 - 1), 1), "%\n", sep = "")
    invisible(x)
}

plot.btest <- function(x, y = NULL, type = "l",
                       xlab = "", ylab = "", ...) {
    if (!is.null(x$timestamp))
        plot(x$timestamp[-seq_len(x$b)], x$wealth[-seq_len(x$b)],
             type = type, xlab = xlab, ylab = ylab, ...)
    else
        plot(x$wealth[-seq_len(x$b)], y,
             type = type, xlab = xlab, ylab = ylab, ...)
    invisible()
}

lines.btest <- function(x, y = NULL, type = "l",
                        xlab = "", ylab = "", ...) {
    if (!is.null(x$timestamp))
        lines(x$timestamp[-seq_len(x$b)], x$wealth[-seq_len(x$b)],
              type = type, xlab = xlab, ylab = ylab, ...)
    else
        lines(x$wealth[-seq_len(x$b)], y,
              type = type, xlab = xlab, ylab = ylab, ...)
    invisible()
}

atest <- btest
formals(atest)$do.signal <- FALSE
formals(atest)$do.rebalance <- FALSE
formals(atest)$final.position <- TRUE
