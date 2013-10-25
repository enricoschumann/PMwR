## -*- truncate-lines: t; -*-
## Time-stamp: <2013-10-25 14:07:49 CEST (es)>
btest  <- function(prices,              
                   signal,              ## a function
                   do.signal = TRUE,    ## a function
                   do.rebalance = TRUE, ## a function
                   print.info = NULL,   ##
                   b = 1L,              ## burnin
                   phi = 1,             ## how much to rebalance
                   initial.position = 0,## initial portfolio
                   initial.cash = 0, ## initial cash
                   tc = 0,
                   ...,
                   add = FALSE,   ## if TRUE, 'position' is flow
                   adjust.signal = NULL,
                   tradeOnOpen = TRUE,
                   tol = 1e-5,
                   assignInGlobals = list(),
                   prices0 = NULL,
                   include.data = FALSE) {

    if (isdebugged(signal))
        db.signal <- TRUE
    else
        db.signal <- FALSE

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

    
    if (is.null(do.signal) || identical(do.signal, TRUE)) {
        do.signal <- function(...)
            TRUE
    } else if (identical(do.signal, FALSE)) {
        do.signal <- function(...)
            FALSE
        warning(sQuote("do.signal"), " is FALSE: strategy will never trade")
    }

    if (is.null(do.rebalance) || identical(do.rebalance, TRUE)) {
        do.rebalance <- function(...)
            TRUE
    } else if (identical(do.rebalance, FALSE)) {
        do.rebalance <- function(...)
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
    funs <- c("signal", "do.signal", "do.rebalance", "print.info")
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
    
    formals(do.rebalance) <-
        c(formals(do.rebalance), alist(Open = Open,
                                      High = High,
                                      Low = Low,
                                      Close = Close,
                                      Wealth = Wealth,
                                      Cash = Cash,
                                      Time = Time,
                                      Portfolio = Portfolio,
                                      SuggestedPortfolio = SuggestedPortfolio,
                                      Globals = Globals))

    if (db.do.rebalance)
        debug(do.rebalance)

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
    
    if (is.null(adjust.signal))
        adjSignal <- FALSE
    else {
        if (adjust.signal != "weight")
            stop(sQuote("adjust.signal"), " must be NULL or weight")
        adjSignal <- TRUE
    }
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
    v <- cash <- numeric(T)
    v[] <- NA
    if (b > 0L) {
        Xs[b,] <- X[b, ] <- initial.position
        cash[b] <- initial.cash
        v[b] <- initial.cash + if (initial.position != 0)
                                   initial.position %*% mC[b, ] else 0
    }

    ## initial wealth
    if (initial.position != 0 && !is.null(prices0)) {
        initial.wealth <- sum(prices0 * initial.position) +initial.cash
    } else if (initial.position != 0) {
        initial.wealth <- initial.cash ## FIXME
    } else
        initial.wealth <- initial.cash

    ## period 1: code is only used if  b == 0L
    if (b == 0L) {
        t <- 1L
        computeSignal <- do.signal(...,
                                   Open = Open, High = High,
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
            
            if (!is.null(adjust.signal))
                temp <- temp * initial.wealth /prices0
           
            if (!is.null(temp))
                Xs[t, ] <- temp
            else
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
                               Time = Time, Portfolio = Portfolio,
                               SuggestedPortfolio = SuggestedPortfolio,
                               Globals = Globals)

        dXs <- Xs[t, ] - if (any(initial.position != 0))
                             initial.position else 0

        if (max(abs(dXs)) < tol)
            rebalance <- FALSE

        if (rebalance) {
            dx <- phi * dXs

            if (tradeOnOpen) ## will convert m(O|C) to vector (drop = TRUE)
                open <- mO[t, ]
            else open <-
                mC[t, ]

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

        v[t] <- X[t, ] %*% mC[t, ] + cash[t]
        if (doPrintInfo)
            print.info(..., Open = Open, High = High, Low = Low,
                      Close = Close, Wealth = Wealth, Cash = Cash,
                      Time = Time, Portfolio = Portfolio,
                      SuggestedPortfolio = SuggestedPortfolio,
                      Globals = Globals)
    }
    ## end period 1


    
    for (t in max(2L, b+1L):T) {
        t1 <- t - 1L        
        computeSignal <- do.signal(...,
                                   Open = Open, High = High,
                                   Low = Low, Close = Close,
                                   Wealth = Wealth,
                                   Cash = Cash,
                                   Time = Time,
                                   Portfolio = Portfolio,
                                   SuggestedPortfolio = SuggestedPortfolio,
                                   Globals = Globals)

        if (computeSignal) {
            temp <- signal(..., Open = Open, High = High,
                           Low = Low, Close = Close, Wealth = Wealth,
                           Cash = Cash, Time = Time,
                           Portfolio = Portfolio,
                           SuggestedPortfolio = SuggestedPortfolio,
                           Globals = Globals)            
            if (!is.null(adjust.signal))
                temp <- temp * v[t1] / mC[t1, ]

            if (!is.null(temp))
                Xs[t, ] <- temp
            else
                Xs[t, ] <- Xs[t1, ] ## b0
            computeSignal <- FALSE
        } else {
            Xs[t, ] <- Xs[t1, ] ## b0
        }


        rebalance <- do.rebalance(..., Open = Open, High = High,
                                  Low = Low, Close = Close,
                                  Wealth = Wealth, Cash = Cash,
                                  Time = Time, Portfolio = Portfolio,
                                  SuggestedPortfolio = SuggestedPortfolio,
                                  Globals = Globals)
        
        dXs <- Xs[t, ] - X[t1, ]  ## b0
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

    ## TODO multiple assets
    trades <- diff(rbind(initial.position, X))
    keep <- abs(trades) > sqrt(.Machine$double.eps) & !is.na(trades)
    keep <- apply(as.matrix(keep), 1, any)
    if (sum(keep))
        jnl <- journal(timestamp = seq_len(NROW(X))[keep],
                      amount = as.matrix(trades[keep, ]),
                      price = mC[keep, ])
    else
        jnl <- journal()
    
    ans <- list(position = X,
                suggested.position = Xs,
                cash = cash,
                wealth = v,
                cum.tc = tccum,
                journal = jnl,
                initial.wealth = initial.wealth)

    if (include.data)
        ans <- c(ans,
                 prices = prices,              
                 signal = signal,
                 do.signal = do.signal,
                 call = match.call())

    ans
}


