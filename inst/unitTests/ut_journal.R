## -*- truncate-lines: t; -*-

test.journal <- function() {

    require("PMwR")
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
    sort(jj)
    sort(jj, decreasing = TRUE)
    
    ## method: length
    checkEquals(length(jj), 10L)
    
##

}
