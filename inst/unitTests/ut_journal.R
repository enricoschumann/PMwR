## -*- truncate-lines: t; -*-

test.journal <- function() {

    require("PMwR")
    require("RUnit")
    
    timestamp <- 1:5
    amount <- 1
    price <- 101:105
    instrument <- "Stock A"
    

    ## journal
    j <- journal(timestamp,amount, price, instrument = instrument)

    checkTrue(is.null(j$account))
    checkTrue(is.null(j$id))


    ## method: c
    jj <- c(j, j)

    ## method: sort
    checkEquals(sort(jj)$timestamp, rep(1:5, each = 2))
    checkEquals(sort(jj, decreasing = TRUE)$timestamp, rep(5:1, each = 2))
    
    ## method: length
    checkEquals(length(jj), 10L)
    
    ##

}
