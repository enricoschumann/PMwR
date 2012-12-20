if (FALSE) {
    backtest <- function(prices,         ### matrices
                         signal,         ### a function
                         signalYN = TRUE, ### a function
                         rebalanceYN = TRUE, ###
                         printInfo = NULL,   ###
                         b = 1L,             ### burnin
                         phi = 1,            ### how much to rebalance
                         x0 = 0,             ### initial portfolio
                         c0 = 100,           ### initial cash
                         tc = 0, ...,
                         adjustSignal = NULL,
                         positionSize = 1,
                         tradeOnOpen = TRUE,
                         tol = 1e-5) {

        ## TODO: checks

        ## TODO: write useful tests

        getOpen <- function(lag = 1L)
            mO[t - lag, ,drop = FALSE]
        getHigh <- function(lag = 1L)
            mH[t - lag, ,drop = FALSE]
        getLow <- function(lag = 1L)
            mL[t - lag, ,drop = FALSE]
        getClose <- function(lag = 1L)
            mC[t - lag, ,drop = FALSE]

        doSignalYN <- TRUE
        if (is.null(signalYN)) {
            signalYN <- function() TRUE
            doSignalYN <- FALSE
        } else if (identical(signalYN, TRUE)) {
            signalYN <- function() TRUE
            doSignalYN <- TRUE
        } else if (identical(signalYN, FALSE)) {
            signalYN <- function() FALSE
            warning("'signalYN' is FALSE: this strategy will never trade")
        }

        doRebalanceYN <- TRUE
        if (is.null(rebalanceYN)) {
            rebalanceYN <- function() TRUE
        } else if (identical(rebalanceYN, TRUE)) {
            rebalanceYN <- function() TRUE
        } else if (identical(rebalanceYN, FALSE)) {
            rebalanceYN <- function() FALSE
        }

        doPrintInfo <- TRUE
        if (is.null(printInfo)) {
            doprintInfo <- FALSE
            printInfo <- function() NULL
        }
        if (is.null(adjustSignal))
            adjSignal <- FALSE else adjSignal <- TRUE

        ## assign the ... arguments
        vars <- list(...)
        if (length(vars)) {
            for(i in seq_len(length(vars)))
                assign(names(vars)[i], vars[[i]])
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
            } else {
                stop("see documentation on 'prices'")
            }
        }
        rm(prices)

        environment(signalYN) <- environment()
        environment(rebalanceYN) <- environment()
        environment(signal) <- environment()
        environment(printInfo) <- environment()

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

        X[b, ] <- x0
        cash <- numeric(T)
        cash[b] <- c0
        v <- numeric(T)
        v[] <- NA
        v[b] <- c0 + x0 %*% mC[b, ]

        for ( t in (b+1L):T ) {
            t1 <- t-1L

            ## COMPUTE SIGNAL?
            computeSignal <- signalYN()

            if (computeSignal) {
                temp <- signal()
                if (adjSignal) {
                    switch(adjustSignal,
                           fixedPosition = {
                               temp <- positionSize *  temp
                           },
                           fixedWeight = {
                               temp <- positionSize * temp * v[t1] / mC[t1, ]
                           },
                           variableWeight = {
                               temp <- temp * v[t1] / mC[t1, ]
                           },
                           stop("unknown value for 'adjustSignal'")
                           ) ### end switch
                }
                Xs[t, ] <- temp
                computeSignal <- FALSE
            } else {
                Xs[t, ] <- Xs[t1, ]
            }

            ## REBALANCE?
            rebalance <- rebalanceYN()

            dXs <- Xs[t, ] - Xs[t1, ]
            if ( max(abs(dXs)) < tol ) rebalance <- FALSE

            if (rebalance) {
                dx <- phi * dXs
                if (tradeOnOpen) {
                    open <- mO[t, ] ### will convert to vector (drop = TRUE)
                } else {
                    open <- mC[t, ] ### will convert to vector (drop = TRUE)
                }
                sx <- dx %*% open
                abs_sx <- (abs(dx) * tc) %*% open
                tccum[t] <- tccum[t1] + abs_sx
                cash[t] <- cash[t1] - sx - abs_sx
                X[t, ] <- X[t1, ] + dx
                rebalance <- FALSE
            } else {
                tccum[t] <- tccum[t1]
                cash[t] <- cash[t1]
                X[t, ] <- X[t1, ]
            }

            ## WEALTH
            v[t] <- X[t, ] %*% mC[t, ] + cash[t]
            if (doPrintInfo) printInfo()
        } ### end of loop

        ## return list of suggested and actual positions
        list(X = X, Xs = Xs, cash = cash, v = v, tccum = tccum)
    }
}
