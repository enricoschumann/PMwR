backtest  <- function(prices,             ## matrices
                      signal,             ## a function
                      signalYN = TRUE,    ## a function
                      rebalanceYN = TRUE, ##
                      printInfo = NULL,   ##
                      b = 1L,             ## burnin
                      phi = 1,            ## how much to rebalance
                      x0 = 0,             ## initial portfolio
                      c0 = 100,           ## initial cash
                      tc = 0,
                      ...,
                      addToPosition = FALSE, ## if TRUE, 'signal' is flow
                      adjustSignal = NULL,
                      positionSize = NULL,
                      tradeOnOpen = TRUE,
                      tol = 1e-5,
                      assignInGlobals = list(),
                      prices0 = NULL) {

    ## TODO: checks

    ## TODO: write useful tests

    ## TXTsignal <- deparse(body(signal))
    ## junk <- gregexpr("Close\\([^\\)]+\\)", TXTsignal,
    ##                  useBytes = FALSE)

    ## for (i in seq_len(length(junk[[1L]])))
    ##     substr(TXTsignal,
    ##            junk[[1]][[i]] + 6,
    ##            junk[[1]][[i]] + attr(junk[[1]], "match.length")[i]-2L)

    if (isdebugged(signal))
        dd.signal <- TRUE else dd.signal <- FALSE
    if (is.function(signalYN) && isdebugged(signalYN))
        dd.signalYN <- TRUE else dd.signalYN <- FALSE
    
    ##
    doSignalYN <- TRUE
    if (is.null(signalYN)) {
        signalYN <- function(...) TRUE
        doSignalYN <- FALSE
    } else if (identical(signalYN, TRUE)) {
        signalYN <- function(...) TRUE
        doSignalYN <- TRUE
    } else if (identical(signalYN, FALSE)) {
        signalYN <- function(...) FALSE
        warning("'signalYN' is FALSE: strategy will never trade")
    }

    ##
    doRebalanceYN <- TRUE
    if (is.null(rebalanceYN)) {
        rebalanceYN <- function(...) TRUE
    } else if (identical(rebalanceYN, TRUE)) {
        rebalanceYN <- function(...) TRUE
    } else if (identical(rebalanceYN, FALSE)) {
        rebalanceYN <- function(...) FALSE
    }

    ##
    doPrintInfo <- TRUE
    if (is.null(printInfo)) {
        doPrintInfo <- FALSE
        printInfo <- function(...)
            NULL
    }

    ## variables to be available in functions
    Open <- function(lag = 1L)
        mO[t - lag, , drop = FALSE]
    High <- function(lag = 1L)
        mH[t - lag, , drop = FALSE]
    Low <- function(lag = 1L)
        mL[t - lag, , drop = FALSE]
    Close <- function(lag = 1L)
        mC[t - lag, , drop = FALSE]
    Wealth <- function(lag = 1L)
        v[t - lag]
    Cash <- function(lag = 1L)
        cash[t - lag]
    Time <- function(lag = 1L)
        t - lag
    Portfolio <- function(lag = 1L)
        X[t - lag, , drop = FALSE]
    SuggestedPortfolio <- function(lag = 1L)
        Xs[t - lag, , drop = FALSE]

    ## create Globals
    Globals <- new.env()
    tmp <- names(assignInGlobals)
    for (i in seq_along(tmp))
        assign(tmp[i], assignInGlobals[[i]], envir = Globals)

    ## check reserved names
    reservedNames <- c("Open", "High", "Low", "Close",
                       "Wealth", "Cash", "Time", "Portfolio",
                       "SuggestedPortfolio", "Globals")
    funs <- c("signal", "signalYN", "rebalanceYN", "printInfo")
    for (thisfun in funs) {
        fNames <- names(formals(get(thisfun)))
        for (rname in reservedNames)
            if (rname %in% fNames)
                stop(sQuote(rname), " cannot be used as an argument name for ",
                     sQuote("signal"))
    }

    formals(signal) <-
        c(formals(signal), alist(Open = Open,
                                 High = High,
                                 Low = Low,
                                 Close = Close,
                                 Wealth = Wealth,
                                 Cash = Cash,
                                 Time = Time,
                                 Portfolio = Portfolio,
                                 SuggestedPortfolio = SuggestedPortfolio,
                                 Globals = Globals))
    if (dd.signal)
        debug(signal)
    
    formals(signalYN) <-
        c(formals(signalYN), alist(Open = Open,
                                   High = High,
                                   Low = Low,
                                   Close = Close,
                                   Wealth = Wealth,
                                   Cash = Cash,
                                   Time = Time,
                                   Portfolio = Portfolio,
                                   SuggestedPortfolio = SuggestedPortfolio,
                                   Globals = Globals))

    formals(rebalanceYN) <-
        c(formals(rebalanceYN), alist(Open = Open,
                                      High = High,
                                      Low = Low,
                                      Close = Close,
                                      Wealth = Wealth,
                                      Cash = Cash,
                                      Time = Time,
                                      Portfolio = Portfolio,
                                      SuggestedPortfolio = SuggestedPortfolio,
                                      Globals = Globals))

    formals(printInfo) <-
        c(formals(printInfo), alist(Open = Open,
                                    High = High,
                                    Low = Low,
                                    Close = Close,
                                    Wealth = Wealth,
                                    Cash = Cash,
                                    Time = Time,
                                    Portfolio = Portfolio,
                                    SuggestedPortfolio = SuggestedPortfolio,
                                    Globals = Globals))

    if (is.null(adjustSignal))
        adjSignal <- FALSE else adjSignal <- TRUE


    ## prepare prices
    if (is.list(prices)) {
        if (length(prices) == 1L) {
            mC <- as.matrix(prices[[1L]])
            tradeOnOpen <- FALSE
        } else if (length(prices) == 4L) {
            mO <- as.matrix(prices[[1L]])
            mH <- as.matrix(prices[[2L]])
            mL <- as.matrix(prices[[3L]])
            mC <- as.matrix(prices[[4L]])
        } else {
            stop("see documentation on 'prices'")
        }
    } else {
        prices <- as.matrix(prices)
        if (ncol(prices) == 1L) {
            mC <- as.matrix(prices)
            tradeOnOpen <- FALSE
        } else if (ncol(prices) == 4L) {
            mO <- as.matrix(prices[ ,1L])
            mH <- as.matrix(prices[ ,2L])
            mL <- as.matrix(prices[ ,3L])
            mC <- as.matrix(prices[ ,4L])
        } else
            stop("see documentation on 'prices'")
    }
    rm(prices)

    ## param .... settings
    T <- nrow(mC)
    nA <- ncol(mC)

    ## tc can be of length nA or length 1L
    tccum <- numeric(T)

    ## X  .... actual portfolios over time
    ## Xs .... signals (recommended)
    X  <- array(NA, dim = c(T, nA))
    Xs <- array( 0, dim = c(T, nA))
    colnames(X) <- colnames(mC)
    colnames(Xs) <- colnames(mC)

    if (b > 0L)
        X[b, ] <- x0
    cash <- numeric(T)

    if (b > 0L)
        cash[b] <- c0
    v <- numeric(T)
    v[] <- NA
    if (b > 0L)
        v[b] <- c0 + ifelse(!identical(0, x0), x0 %*% mC[b, ], 0 )

    ## initial wealth
    if (x0 != 0 && !is.null(prices0)) {
        initial.wealth <- sum(prices0 * x0) +c0
    } else if (x0 != 0) {
        initial.wealth <- c0 ## FIXME
    } else
        initial.wealth <- c0

    ## period 1 (only if b == 0L)
    if (b == 0L) {
        t <- 1L
        ## COMPUTE SIGNAL?
        computeSignal <- signalYN(..., Open = Open, High = High,
                                  Low = Low, Close = Close,
                                  Wealth = Wealth, Cash = Cash,
                                  Time = Time, Portfolio = Portfolio,
                                  SuggestedPortfolio = SuggestedPortfolio,
                                  Globals = Globals)

        if (computeSignal) {

            temp <- signal(..., Open = Open, High = High, Low = Low,
                           Close = Close, Wealth = Wealth, Cash = Cash,
                           Time = Time, Portfolio = Portfolio,
                           SuggestedPortfolio = SuggestedPortfolio,
                           Globals = Globals)

            if (adjSignal) {
                switch(adjustSignal,
                       fixedPosition = temp <- positionSize *  temp,
                       weight = {
                           if (!is.null(positionSize)) {
                               temp <- positionSize * sign(temp) * initial.wealth / prices0
                           } else temp <- temp * initial.wealth /prices0
                       },
                       stop("unknown value for 'adjustSignal'"))}

            if (!is.null(temp))
                Xs[t, ] <- temp
            else
                Xs[t, ] <- rep.int(0, nA)
            computeSignal <- FALSE
        } else {
            Xs[t, ] <- rep.int(0, nA)
        }

        ## REBALANCE?
        rebalance <- rebalanceYN(..., Open = Open, High = High,
                                 Low = Low, Close = Close,
                                 Wealth = Wealth, Cash = Cash,
                                 Time = Time, Portfolio = Portfolio,
                                 SuggestedPortfolio = SuggestedPortfolio,
                                 Globals = Globals)

        dXs <- Xs[t, ] - ifelse(x0 != 0, x0, 0)
        if (max(abs(dXs)) < tol)
            rebalance <- FALSE

        if (rebalance) {
            dx <- phi * dXs

            if (tradeOnOpen) ## will convert m(O|C) to vector (drop = TRUE)
                open <- mO[t, ]  else open <- mC[t, ]

            sx <- dx %*% open
            abs_sx <- (abs(dx) * tc) %*% open
            tccum[t] <- abs_sx
            cash[t] <- c0 - sx - abs_sx
            X[t, ] <- ifelse(x0 != 0, x0, 0) + dx
            rebalance <- FALSE
        } else {
            tccum[t] <- 0
            cash[t] <- c0
            X[t, ] <- ifelse(x0 != 0, x0, 0)
        }

        v[t] <- X[t, ] %*% mC[t, ] + cash[t]
        if (doPrintInfo)
            printInfo(..., Open = Open, High = High, Low = Low,
                      Close = Close, Wealth = Wealth, Cash = Cash,
                      Time = Time, Portfolio = Portfolio,
                      SuggestedPortfolio = SuggestedPortfolio,
                      Globals = Globals)
    }
    ## end period 1

    strt <-max(2L, b+1L)
    for ( t in strt:T ) {

        t1 <- t-1L

        ## COMPUTE SIGNAL?
        computeSignal <- signalYN(..., Open = Open, High = High,
                                  Low = Low, Close = Close,
                                  Wealth = Wealth, Cash = Cash,
                                  Time = Time, Portfolio = Portfolio,
                                  SuggestedPortfolio = SuggestedPortfolio,
                                  Globals = Globals)

        if (computeSignal) {
            temp <- signal(..., Open = Open, High = High,
                           Low = Low, Close = Close, Wealth = Wealth,
                           Cash = Cash, Time = Time,
                           Portfolio = Portfolio,
                           SuggestedPortfolio = SuggestedPortfolio,
                           Globals = Globals)

            if (adjSignal) {
                switch(adjustSignal,
                       fixedPosition = {
                           temp <- positionSize *  temp
                       },
                       weight = {
                           if (!is.null(positionSize)) {
                               temp <- positionSize * sign(temp) * v[t1] / mC[t1, ]
                           } else temp <- temp * v[t1] / mC[t1, ]
                       },
                       stop("unknown value for 'adjustSignal'")
                       ) ## end switch
            }
            if (!is.null(temp))
                Xs[t, ] <- temp
            else
                Xs[t, ] <- Xs[t1, ] ## b0
            computeSignal <- FALSE
        } else {
            Xs[t, ] <- Xs[t1, ] ## b0
        }

        ## REBALANCE?
        rebalance <- rebalanceYN(..., Open = Open, High = High,
                                 Low = Low, Close = Close,
                                 Wealth = Wealth, Cash = Cash,
                                 Time = Time, Portfolio = Portfolio,
                                 SuggestedPortfolio = SuggestedPortfolio,
                                 Globals = Globals)

        dXs <- Xs[t, ] - Xs[t1, ]  ## b0
        if (max(abs(dXs)) < tol)
            rebalance <- FALSE

        if (rebalance) {
            dx <- phi * dXs

            if (tradeOnOpen) ## will convert m(O|C) to vector (drop = TRUE)
                open <- mO[t, ]  else open <- mC[t, ]

            sx <- dx %*% open
            abs_sx <- (abs(dx) * tc) %*% open
            tccum[t] <- tccum[t1] + abs_sx
            cash[t] <- cash[t1] - sx - abs_sx
            X[t, ] <- X[t1, ] + dx  ## b0
            rebalance <- FALSE
        } else {
            tccum[t] <- tccum[t1]## b0
            cash[t] <- cash[t1]## b0
            X[t, ] <- X[t1, ]  ## b0
        }

        ## WEALTH
        v[t] <- X[t, ] %*% mC[t, ] + cash[t]
        if (doPrintInfo)
            printInfo(...,
                      Open = Open,
                      High = High,
                      Low = Low,
                      Close = Close,
                      Wealth = Wealth,
                      Cash = Cash,
                      Time = Time,
                      Portfolio = Portfolio,
                      SuggestedPortfolio = SuggestedPortfolio,
                      Globals = Globals)
    } ## end of for loop

    tmp <- list(times = 1:NROW(X), trades = diff(rbind(x0,X)))
    keep <- abs(tmp$trades) > sqrt(.Machine$double.eps) & !is.na(tmp$trades)
    keep <- apply(as.matrix(keep), 1, function(x) any(x))
    ## TODO: add trade prices
    trades <- list(times = tmp$times[keep],
                   notional = as.matrix(tmp$trades[keep, ]), prices = NA)

    list(portfolio = X,
         suggested.portfolio = Xs,
         cash = cash,
         wealth = v,
         cumTC = tccum,
         trades = trades,
         initial.wealth = initial.wealth)
}


btest  <- function(prices,             ## matrices
                   signal,             ## a function
                   signalYN = TRUE,    ## a function
                   rebalanceYN = TRUE, ##
                   printInfo = NULL,   ##
                   b = 1L,             ## burnin
                   phi = 1,            ## how much to rebalance
                   x0 = 0,             ## initial portfolio
                   c0 = 100,           ## initial cash
                   tc = 0,
                   ...,
                   addToPosition = FALSE, ## if TRUE, 'signal' is flow
                   adjustSignal = NULL,
                   positionSize = NULL,
                   tradeOnOpen = TRUE,
                   tol = 1e-5,
                   assignInGlobals = list(),
                   prices0 = NULL) {

    ## TODO: checks

    ## TODO: write useful tests

    ## TXTsignal <- deparse(body(signal))
    ## junk <- gregexpr("Close\\([^\\)]+\\)", TXTsignal,
    ##                  useBytes = FALSE)

    ## for (i in seq_len(length(junk[[1L]])))
    ##     substr(TXTsignal,
    ##            junk[[1]][[i]] + 6,
    ##            junk[[1]][[i]] + attr(junk[[1]], "match.length")[i]-2L)

    if (isdebugged(signal))
        dd.signal <- TRUE else dd.signal <- FALSE
    if (is.function(signalYN) && isdebugged(signalYN))
        dd.signalYN <- TRUE else dd.signalYN <- FALSE
    
    ##
    doSignalYN <- TRUE
    if (is.null(signalYN)) {
        signalYN <- function(...) TRUE
        doSignalYN <- FALSE
    } else if (identical(signalYN, TRUE)) {
        signalYN <- function(...) TRUE
        doSignalYN <- TRUE
    } else if (identical(signalYN, FALSE)) {
        signalYN <- function(...) FALSE
        warning("'signalYN' is FALSE: strategy will never trade")
    }

    ##
    doRebalanceYN <- TRUE
    if (is.null(rebalanceYN)) {
        rebalanceYN <- function(...) TRUE
    } else if (identical(rebalanceYN, TRUE)) {
        rebalanceYN <- function(...) TRUE
    } else if (identical(rebalanceYN, FALSE)) {
        rebalanceYN <- function(...) FALSE
    }

    ##
    doPrintInfo <- TRUE
    if (is.null(printInfo)) {
        doPrintInfo <- FALSE
        printInfo <- function(...)
            NULL
    }

    ## variables to be available in functions
    Open <- function(lag = 1L)
        mO[t - lag, , drop = FALSE]
    High <- function(lag = 1L)
        mH[t - lag, , drop = FALSE]
    Low <- function(lag = 1L)
        mL[t - lag, , drop = FALSE]
    Close <- function(lag = 1L)
        mC[t - lag, , drop = FALSE]
    Wealth <- function(lag = 1L)
        v[t - lag]
    Cash <- function(lag = 1L)
        cash[t - lag]
    Time <- function(lag = 1L)
        t - lag
    Portfolio <- function(lag = 1L)
        X[t - lag, , drop = FALSE]
    SuggestedPortfolio <- function(lag = 1L)
        Xs[t - lag, , drop = FALSE]

    ## create Globals
    Globals <- new.env()
    tmp <- names(assignInGlobals)
    for (i in seq_along(tmp))
        assign(tmp[i], assignInGlobals[[i]], envir = Globals)

    ## check reserved names
    reservedNames <- c("Open", "High", "Low", "Close",
                       "Wealth", "Cash", "Time", "Portfolio",
                       "SuggestedPortfolio", "Globals")
    funs <- c("signal", "signalYN", "rebalanceYN", "printInfo")
    for (thisfun in funs) {
        fNames <- names(formals(get(thisfun)))
        for (rname in reservedNames)
            if (rname %in% fNames)
                stop(sQuote(rname), " cannot be used as an argument name for ",
                     sQuote("signal"))
    }

    formals(signal) <-
        c(formals(signal), alist(Open = Open,
                                 High = High,
                                 Low = Low,
                                 Close = Close,
                                 Wealth = Wealth,
                                 Cash = Cash,
                                 Time = Time,
                                 Portfolio = Portfolio,
                                 SuggestedPortfolio = SuggestedPortfolio,
                                 Globals = Globals))
    if (dd.signal)
        debug(signal)
    
    formals(signalYN) <-
        c(formals(signalYN), alist(Open = Open,
                                   High = High,
                                   Low = Low,
                                   Close = Close,
                                   Wealth = Wealth,
                                   Cash = Cash,
                                   Time = Time,
                                   Portfolio = Portfolio,
                                   SuggestedPortfolio = SuggestedPortfolio,
                                   Globals = Globals))

    formals(rebalanceYN) <-
        c(formals(rebalanceYN), alist(Open = Open,
                                      High = High,
                                      Low = Low,
                                      Close = Close,
                                      Wealth = Wealth,
                                      Cash = Cash,
                                      Time = Time,
                                      Portfolio = Portfolio,
                                      SuggestedPortfolio = SuggestedPortfolio,
                                      Globals = Globals))

    formals(printInfo) <-
        c(formals(printInfo), alist(Open = Open,
                                    High = High,
                                    Low = Low,
                                    Close = Close,
                                    Wealth = Wealth,
                                    Cash = Cash,
                                    Time = Time,
                                    Portfolio = Portfolio,
                                    SuggestedPortfolio = SuggestedPortfolio,
                                    Globals = Globals))

    if (is.null(adjustSignal))
        adjSignal <- FALSE else adjSignal <- TRUE


    ## prepare prices
    if (is.list(prices)) {
        if (length(prices) == 1L) {
            mC <- as.matrix(prices[[1L]])
            tradeOnOpen <- FALSE
        } else if (length(prices) == 4L) {
            mO <- as.matrix(prices[[1L]])
            mH <- as.matrix(prices[[2L]])
            mL <- as.matrix(prices[[3L]])
            mC <- as.matrix(prices[[4L]])
        } else {
            stop("see documentation on 'prices'")
        }
    } else {
        prices <- as.matrix(prices)
        if (ncol(prices) == 1L) {
            mC <- as.matrix(prices)
            tradeOnOpen <- FALSE
        } else if (ncol(prices) == 4L) {
            mO <- as.matrix(prices[ ,1L])
            mH <- as.matrix(prices[ ,2L])
            mL <- as.matrix(prices[ ,3L])
            mC <- as.matrix(prices[ ,4L])
        } else
            stop("see documentation on 'prices'")
    }
    rm(prices)

    ## param .... settings
    T <- nrow(mC)
    nA <- ncol(mC)

    ## tc can be of length nA or length 1L
    tccum <- numeric(T)

    ## X  .... actual portfolios over time
    ## Xs .... signals (recommended)
    X  <- array(NA, dim = c(T, nA))
    Xs <- array( 0, dim = c(T, nA))
    colnames(X) <- colnames(mC)
    colnames(Xs) <- colnames(mC)

    if (b > 0L)
        X[b, ] <- x0
    cash <- numeric(T)

    if (b > 0L)
        cash[b] <- c0
    v <- numeric(T)
    v[] <- NA
    if (b > 0L)
        v[b] <- c0 + ifelse(!identical(0, x0), x0 %*% mC[b, ], 0 )

    ## initial wealth
    if (x0 != 0 && !is.null(prices0)) {
        initial.wealth <- sum(prices0 * x0) +c0
    } else if (x0 != 0) {
        initial.wealth <- c0 ## FIXME
    } else
        initial.wealth <- c0

    ## period 1 (only if b == 0L)
    if (b == 0L) {
        t <- 1L
        ## COMPUTE SIGNAL?
        computeSignal <- signalYN(..., Open = Open, High = High,
                                  Low = Low, Close = Close,
                                  Wealth = Wealth, Cash = Cash,
                                  Time = Time, Portfolio = Portfolio,
                                  SuggestedPortfolio = SuggestedPortfolio,
                                  Globals = Globals)

        if (computeSignal) {

            temp <- signal(..., Open = Open, High = High, Low = Low,
                           Close = Close, Wealth = Wealth, Cash = Cash,
                           Time = Time, Portfolio = Portfolio,
                           SuggestedPortfolio = SuggestedPortfolio,
                           Globals = Globals)

            if (adjSignal) {
                switch(adjustSignal,
                       fixedPosition = temp <- positionSize *  temp,
                       weight = {
                           if (!is.null(positionSize)) {
                               temp <- positionSize * sign(temp) * initial.wealth / prices0
                           } else temp <- temp * initial.wealth /prices0
                       },
                       stop("unknown value for 'adjustSignal'"))}

            if (!is.null(temp))
                Xs[t, ] <- temp
            else
                Xs[t, ] <- rep.int(0, nA)
            computeSignal <- FALSE
        } else {
            Xs[t, ] <- rep.int(0, nA)
        }

        ## REBALANCE?
        rebalance <- rebalanceYN(..., Open = Open, High = High,
                                 Low = Low, Close = Close,
                                 Wealth = Wealth, Cash = Cash,
                                 Time = Time, Portfolio = Portfolio,
                                 SuggestedPortfolio = SuggestedPortfolio,
                                 Globals = Globals)

        dXs <- Xs[t, ] - ifelse(x0 != 0, x0, 0)
        if (max(abs(dXs)) < tol)
            rebalance <- FALSE

        if (rebalance) {
            dx <- phi * dXs

            if (tradeOnOpen) ## will convert m(O|C) to vector (drop = TRUE)
                open <- mO[t, ]  else open <- mC[t, ]

            sx <- dx %*% open
            abs_sx <- (abs(dx) * tc) %*% open
            tccum[t] <- abs_sx
            cash[t] <- c0 - sx - abs_sx
            X[t, ] <- ifelse(x0 != 0, x0, 0) + dx
            rebalance <- FALSE
        } else {
            tccum[t] <- 0
            cash[t] <- c0
            X[t, ] <- ifelse(x0 != 0, x0, 0)
        }

        v[t] <- X[t, ] %*% mC[t, ] + cash[t]
        if (doPrintInfo)
            printInfo(..., Open = Open, High = High, Low = Low,
                      Close = Close, Wealth = Wealth, Cash = Cash,
                      Time = Time, Portfolio = Portfolio,
                      SuggestedPortfolio = SuggestedPortfolio,
                      Globals = Globals)
    }
    ## end period 1

    strt <-max(2L, b+1L)
    for ( t in strt:T ) {

        t1 <- t-1L

        ## COMPUTE SIGNAL?
        computeSignal <- signalYN(..., Open = Open, High = High,
                                  Low = Low, Close = Close,
                                  Wealth = Wealth, Cash = Cash,
                                  Time = Time, Portfolio = Portfolio,
                                  SuggestedPortfolio = SuggestedPortfolio,
                                  Globals = Globals)

        if (computeSignal) {
            temp <- signal(..., Open = Open, High = High,
                           Low = Low, Close = Close, Wealth = Wealth,
                           Cash = Cash, Time = Time,
                           Portfolio = Portfolio,
                           SuggestedPortfolio = SuggestedPortfolio,
                           Globals = Globals)

            if (adjSignal) {
                switch(adjustSignal,
                       fixedPosition = {
                           temp <- positionSize *  temp
                       },
                       weight = {
                           if (!is.null(positionSize)) {
                               temp <- positionSize * sign(temp) * v[t1] / mC[t1, ]
                           } else temp <- temp * v[t1] / mC[t1, ]
                       },
                       stop("unknown value for 'adjustSignal'")
                       ) ## end switch
            }
            if (!is.null(temp))
                Xs[t, ] <- temp
            else
                Xs[t, ] <- Xs[t1, ] ## b0
            computeSignal <- FALSE
        } else {
            Xs[t, ] <- Xs[t1, ] ## b0
        }

        ## REBALANCE?
        rebalance <- rebalanceYN(..., Open = Open, High = High,
                                 Low = Low, Close = Close,
                                 Wealth = Wealth, Cash = Cash,
                                 Time = Time, Portfolio = Portfolio,
                                 SuggestedPortfolio = SuggestedPortfolio,
                                 Globals = Globals)

        dXs <- Xs[t, ] - Xs[t1, ]  ## b0
        if (max(abs(dXs)) < tol)
            rebalance <- FALSE

        if (rebalance) {
            dx <- phi * dXs

            if (tradeOnOpen) ## will convert m(O|C) to vector (drop = TRUE)
                open <- mO[t, ]  else open <- mC[t, ]

            sx <- dx %*% open
            abs_sx <- (abs(dx) * tc) %*% open
            tccum[t] <- tccum[t1] + abs_sx
            cash[t] <- cash[t1] - sx - abs_sx
            X[t, ] <- X[t1, ] + dx  ## b0
            rebalance <- FALSE
        } else {
            tccum[t] <- tccum[t1]## b0
            cash[t] <- cash[t1]## b0
            X[t, ] <- X[t1, ]  ## b0
        }

        ## WEALTH
        v[t] <- X[t, ] %*% mC[t, ] + cash[t]
        if (doPrintInfo)
            printInfo(...,
                      Open = Open,
                      High = High,
                      Low = Low,
                      Close = Close,
                      Wealth = Wealth,
                      Cash = Cash,
                      Time = Time,
                      Portfolio = Portfolio,
                      SuggestedPortfolio = SuggestedPortfolio,
                      Globals = Globals)
    } ## end of for loop

    tmp <- list(times = 1:NROW(X), trades = diff(rbind(x0,X)))
    keep <- abs(tmp$trades) > sqrt(.Machine$double.eps) & !is.na(tmp$trades)
    keep <- apply(as.matrix(keep), 1, function(x) any(x))
    ## TODO: add trade prices
    trades <- list(times = tmp$times[keep],
                   notional = as.matrix(tmp$trades[keep, ]), prices = NA)

    list(portfolio = X,
         suggested.portfolio = Xs,
         cash = cash,
         wealth = v,
         cumTC = tccum,
         trades = trades,
         initial.wealth = initial.wealth)
}


