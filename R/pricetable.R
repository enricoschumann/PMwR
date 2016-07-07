## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

pricetable <- function(instrument, when, price,
                       price.instrument = NULL, price.timestamp = NULL,
                       missing = "error") {

    if (!is.matrix(price))
        price <- as.matrix(price)
    
    if (is.null(price.instrument) && !is.null(colnames(price)))
        price.instrument <- colnames(price)
    
    if (missing(instrument)) {
        ## no instrument
        ## ==> compute prices for each column in price
        if (!is.null(price.instrument))
            instrument <- price.instrument
        else if (is.null(price.instrument))
            price.instrument <-
                instrument  <- paste0("i", seq_len(ncol(price)))
    }
    
    if (is.null(price.timestamp) && !is.null(rownames(price)))
        price.timestamp <- rownames(price)
    else if (is.null(price.timestamp))
        price.timestamp <- seq_len(nrow(price))
    

    if (length(miss <- setdiff(instrument, price.instrument))) {
        if (is.na(missing) || missing == "NA") {
            tmp <- array(NA, dim = c(nrow(price), length(miss)))
            colnames(tmp) <- miss
            price <- cbind(price, tmp)            
        } else if (tolower(missing) == "error") {
            stop("no data for instrument(s) ", paste(miss, collapse = ", "))
        } else {
            tmp <- array(missing, dim = c(nrow(price), length(miss)))
            colnames(tmp) <- miss
            price <- cbind(price, tmp)
        }
    }
        
    i <- matchOrPrevious(when, price.timestamp)
    j <- match(instrument, price.instrument)
    ans <- price[i, j, drop = FALSE]
    colnames(ans) <- instrument
    rownames(ans) <- as.character(when)
    class(ans) <- "pricetable"
    attr(ans, "instrument") <- instrument
    attr(ans, "timestamp")  <- when
    ans
}


## pricetable <- function(when, instrument, ...) {

##     series <- list(...)
##     ns <- length(series)
##     if (ns %% 2 == 1L)
##         stop("need prices and time series")
    
##     ans <- array(NA, dim = c(length(when), ns/2))
    
##     for (i in seq_len(ns/2))
##         ans[, i] <- series[[i*2-1]][matchOrPrevious(when, series[[i*2]])]

##     ans <- list(price.table = ans, timestamp = when, instrument = instrument)
##     class(ans) <- "price_table"
##     ans
## }
