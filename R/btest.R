## -*- truncate-lines: t; -*-
## Time-stamp: <2013-09-16 16:20:28 CEST (es)>
btest  <- function(prices,              ## matrices
                   signal,            ## a function
                   do.signal = TRUE,  ## a function
                   rebalance = TRUE,    ## a function
                   print.info = NULL,    ##
                   b = 1L,              ## burnin
                   phi = 1,             ## how much to rebalance
                   initial.position = 0,              ## initial portfolio
                   initial.cash = 100,            ## initial cash
                   tc = 0,
                   ...,
                   add = FALSE,   ## if TRUE, 'position' is flow
                   adjust.position = NULL,
                   size = NULL,
                   tradeOnOpen = TRUE,
                   tol = 1e-5,
                   assignInGlobals = list(),
                   prices0 = NULL) {

    if (isdebugged(signal))
        db.signal <- TRUE
    else
        db.signal <- FALSE

    if (is.function(do.signal) && isdebugged(do.signal))
        db.do.signal <- TRUE
    else
        db.do.signal <- FALSE

    if (is.function(rebalance) && isdebugged(rebalance))
        db.rebalance <- TRUE
    else
        db.rebalance <- FALSE

    if (is.function(print.info) && isdebugged(print.info))
        db.print.info <- TRUE
    else
        db.print.info <- FALSE

    
    if (is.null(do.signal)) {
        do.signal <- function(...)
            TRUE
    } else if (identical(do.signal, TRUE)) {
        do.signal <- function(...)
            TRUE
    } else if (identical(do.signal, FALSE)) {
        do.signal <- function(...)
            FALSE
        warning(sQuote("do.signal"), " is FALSE: strategy will never trade")
    }

    if (is.null(rebalance)) {
        rebalance <- function(...)
            TRUE
    } else if (identical(rebalance, TRUE)) {
        rebalance <- function(...)
            TRUE
    } else if (identical(rebalance, FALSE)) {
        rebalance <- function(...)
            FALSE
    }

    doPrintInfo <- TRUE
    if (is.null(print.info)) {
        doPrintInfo <- FALSE
        print.info <- function(...)
            NULL
    }

    ## variables to be available in functions
    Open <- function(lag = 1)
        mO[t - lag, , drop = FALSE]
    High <- function(lag = 1)
        mH[t - lag, , drop = FALSE]
    Low <- function(lag = 1)
        mL[t - lag, , drop = FALSE]
    Close <- function(lag = 1)
        mC[t - lag, , drop = FALSE]
    Wealth <- function(lag = 1)
        v[t - lag]
    Cash <- function(lag = 1)
        cash[t - lag]
    Time <- function(lag = 1)
        t - lag
    Portfolio <- function(lag = 1)
        X[t - lag, , drop = FALSE]
    SuggestedPortfolio <- function(lag = 1)
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
    funs <- c("signal", "do.signal", "rebalance", "print.info")
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
    if (db.signal)
        debug(signal)
    
    formals(do.signal) <-
        c(formals(do.signal), alist(Open = Open,
                                   High = High,
                                   Low = Low,
                                   Close = Close,
                                   Wealth = Wealth,
                                   Cash = Cash,
                                   Time = Time,
                                   Portfolio = Portfolio,
                                   SuggestedPortfolio = SuggestedPortfolio,
                                   Globals = Globals))
    if (db.do.signal)
        debug(do.signal)
    
    formals(rebalance) <-
        c(formals(rebalance), alist(Open = Open,
                                      High = High,
                                      Low = Low,
                                      Close = Close,
                                      Wealth = Wealth,
                                      Cash = Cash,
                                      Time = Time,
                                      Portfolio = Portfolio,
                                      SuggestedPortfolio = SuggestedPortfolio,
                                      Globals = Globals))

    if (db.rebalance)
        debug(rebalance)

    formals(print.info) <-
        c(formals(print.info), alist(Open = Open,
                                     High = High,
                                     Low = Low,
                                     Close = Close,
                                     Wealth = Wealth,
                                     Cash = Cash,
                                     Time = Time,
                                     Portfolio = Portfolio,
                                     SuggestedPortfolio = SuggestedPortfolio,
                                     Globals = Globals))

    if (db.print.info)
        debug(print.info)
    
    if (is.null(adjSignal))
        adjSignal <- FALSE
    else
        adjSignal <- TRUE

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
    rm(list = "prices")

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
        X[b, ] <- initial.position
    cash <- numeric(T)

    if (b > 0L)
        cash[b] <- initial.cash
    v <- numeric(T)
    v[] <- NA
    if (b > 0L)
        v[b] <- initial.cash + ifelse(!identical(0, initial.position), initial.position %*% mC[b, ], 0 )

    ## initial wealth
    if (initial.position != 0 && !is.null(prices0)) {
        initial.wealth <- sum(prices0 * initial.position) +initial.cash
    } else if (initial.position != 0) {
        initial.wealth <- initial.cash ## FIXME
    } else
        initial.wealth <- initial.cash

    ## period 1 (only if b == 0L)
    if (b == 0L) {
        t <- 1L
        ## COMPUTE SIGNAL?
        computeSignal <- do.signal(..., Open = Open, High = High,
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
                switch(adjust.position,
                       fixedPosition = temp <- size *  temp,
                       weight = {
                           if (!is.null(size)) {
                               temp <- size * sign(temp) * initial.wealth / prices0
                           } else temp <- temp * initial.wealth /prices0
                       },
                       stop("unknown value for 'adjust.position'"))}

            if (!is.null(temp))
                Xs[t, ] <- temp
            else
                Xs[t, ] <- rep.int(0, nA)
            computeSignal <- FALSE
        } else {
            Xs[t, ] <- rep.int(0, nA)
        }

        ## REBALANCE?
        rebalance <- rebalance(..., Open = Open, High = High,
                                 Low = Low, Close = Close,
                                 Wealth = Wealth, Cash = Cash,
                                 Time = Time, Portfolio = Portfolio,
                                 SuggestedPortfolio = SuggestedPortfolio,
                                 Globals = Globals)

        dXs <- Xs[t, ] - ifelse(initial.position != 0, initial.position, 0)
        if (max(abs(dXs)) < tol)
            rebalance <- FALSE

        if (rebalance) {
            dx <- phi * dXs

            if (tradeOnOpen) ## will convert m(O|C) to vector (drop = TRUE)
                open <- mO[t, ]  else open <- mC[t, ]

            sx <- dx %*% open
            abs_sx <- (abs(dx) * tc) %*% open
            tccum[t] <- abs_sx
            cash[t] <- initial.cash - sx - abs_sx
            X[t, ] <- ifelse(initial.position != 0, initial.position, 0) + dx
            rebalance <- FALSE
        } else {
            tccum[t] <- 0
            cash[t] <- initial.cash
            X[t, ] <- ifelse(initial.position != 0, initial.position, 0)
        }

        v[t] <- X[t, ] %*% mC[t, ] + cash[t]
        if (doPrintInfo)
            print.info(..., Open = Open, High = High, Low = Low,
                      Close = Close, Wealth = Wealth, Cash = Cash,
                      Time = Time, Portfolio = Portfolio,
                      SuggestedPortfolio = SuggestedPortfolio,
                      Globals = Globals)
    }
    ## end period 1

    strt <- max(2L, b+1L)
    for ( t in strt:T ) {

        t1 <- t-1L

        ## COMPUTE SIGNAL?
        computeSignal <- do.signal(..., Open = Open, High = High,
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
                switch(adjust.position,
                       fixedPosition = {
                           temp <- size *  temp
                       },
                       weight = {
                           if (!is.null(size)) {
                               temp <- size * sign(temp) * v[t1] / mC[t1, ]
                           } else temp <- temp * v[t1] / mC[t1, ]
                       },
                       stop("unknown value for 'adjust.position'")
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
        rebalanceYN <- rebalance(..., Open = Open, High = High,
                               Low = Low, Close = Close,
                               Wealth = Wealth, Cash = Cash,
                               Time = Time, Portfolio = Portfolio,
                               SuggestedPortfolio = SuggestedPortfolio,
                               Globals = Globals)

        dXs <- Xs[t, ] - Xs[t1, ]  ## b0
        if (max(abs(dXs)) < tol)
            rebalanceYN <- FALSE
        
        if (rebalanceYN) {
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
            print.info(...,
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

    tmp <- list(times = 1:NROW(X), trades = diff(rbind(initial.position,X)))
    keep <- abs(tmp$trades) > sqrt(.Machine$double.eps) & !is.na(tmp$trades)
    keep <- apply(as.matrix(keep), 1, function(x) any(x))

    list(position = X,
         suggested.position = Xs,
         cash = cash,
         wealth = v,
         cum.tc = tccum,
         journal = journal(timestamp = tmp$times[keep],
                              amount = as.matrix(tmp$trades[keep, ]),
                              price = prices[keep]),
         initial.wealth = initial.wealth)
}


