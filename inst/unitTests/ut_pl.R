test.pl <- function() {

    require("PMwR")
    require("RUnit")
    
    checkEquals(pl(amount = c(1,-1),
                   price  = c(1,2))[[1]][["pnl"]], 1)

    timestamp <- 1:4
    amount <- c(1,1,1,1)
    price <- 101:104
    instrument <- "my I"
    jnl <- journal(timestamp, amount, price, , instrument= instrument)
    checkTrue(is.na(suppressWarnings(pl(jnl)[[1]][["pnl"]])))
    
    ## amount <- c(1,1,-1,1,-1)
    ## price <- c(100,99,101,100,101)
    ## pl(amount, price, along.timestamp = TRUE)
    ## PMwR:::avg(amount, price)
    
    ## amount <- c(1,-2,1)
    ## price <- c(100,101,100)
    ## pl(amount, price)
    ## pl(amount, price, along.timestamp = TRUE)
    ## PMwR:::avg(amount, price)

    ## tmp <- splitTrades(amount, price, timestamp = seq_along(amount),
    ##                    aggregate = TRUE)    
    ## PMwR:::avg(tmp$amount, tmp$price)

    
    ## J <- journal(timestamp = c(1, 2, 3),
    ##              amount = c(1, 1, -2),
    ##              price  = c(100,102, 101))
    ## pl(c(1, 1, -2), c(100,102, 101))

}

