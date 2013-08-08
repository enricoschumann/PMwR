test.pl <- function() {

    require("PMwR")
    require("RUnit")
    
    checkEquals(pl(amount = c(1,-1),
                   price  = c(1,2))[["pl"]], 1)


    timestamp <- 1:4
    amount <- c(1,1,1,1)
    price <- 101:104
    instrument <- "my I"
    jnl <- journal(timestamp, amount, price, , instrument)
    pl(jnl)
    

    amount <- -c(1,1,-1,1,-1)
    price <- c(100,100,101,100,101)

    pl(amount, price, along.timestamp = TRUE)
    PMwR:::avg(amount, price)

}

