## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-
## Time-stamp: <2015-02-13 17:57:49 CET (es)>

## require("database")
## searchInstruments("siemens")

## tmp <- fetchTable("daily2",
##                   c("de0007236101","de000a1ewww0"), "close")

## when <- as.Date(c("2015-1-11","2015-1-14","2015-3-14"))
## series <- list(tmp$close[,1], tmp$timestamp, 
##                tmp$close[,2], tmp$timestamp)

price_table(when, instrument, ...) {

    series <- list(...)
    ns <- length(series)
    if (ns %% 2 == 1L)
        stop("need prices and time series")
    
    ans <- array(NA, dim = c(length(when), ns/2))
    
    for (i in seq_len(ns/2))
        ans[, i] <- series[[i*2-1]][PMwR:::matchOrPrevious(when, series[[i*2]])]

    ans <- list(price.table = ans, timestamp = when, instrument = instrument)
    class(ans) <- "price.table"
    ans
}
