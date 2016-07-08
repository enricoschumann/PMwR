test.pl <- function() {

    require("PMwR")
    require("RUnit")
    
    checkEquals(pl(amount = c(1,-1),
                   price  = c(1,2))[[1]][["pl"]], 1)

    checkEquals(pl(amount = c(1,-1), price = c(1,2)),                
                structure(list(structure(list(pl = 1,
                                              realised = NA,
                                              unrealised = NA, 
                                              buy = 1,
                                              sell = 2,
                                              volume = 2),
                                         .Names = c("pl",
                                                    "realised", "unrealised",
                                                    "buy", "sell", "volume"))),
                          class = "pl", along.timestamp = FALSE, instrument = NA))
                
    checkEquals(pl(amount = 1, price = 1,
                   initial.position = 1, initial.price = 1,
                   eval.price = 2),
                structure(list(structure(list(pl = 2,
                                              realised = NA,
                                              unrealised = NA, 
                                              buy = 1,
                                              sell = 2,
                                              volume = 1),
                                         .Names = c("pl",
                                                    "realised", "unrealised",
                                                    "buy", "sell", "volume"))),
                          class = "pl", along.timestamp = FALSE, instrument = NA))


    x <- pl(amount = c(1,-1, 20,-20),
            price = c(100,102, 4,7),
            instrument = c("Equity A", "Equity A", "Equity B", "Equity B"))

    checkEquals(x,
                structure(list(`Equity A` = structure(list(pl = 2,
                                                           realised = NA, 
                                                           unrealised = NA,
                                                           buy = 100, sell = 102,
                                                           volume = 2),
                                                      .Names = c("pl",
                                                                 "realised", "unrealised",
                                                                 "buy", "sell", "volume")),
                               `Equity B` = structure(list(pl = 60,
                                                           realised = NA,
                                                           unrealised = NA,
                                                           buy = 4, sell = 7, 
                                                           volume = 40),
                                                      .Names = c("pl",
                                                                 "realised", "unrealised",
                                                                 "buy", "sell", "volume"))),
                          class = "pl", along.timestamp = FALSE,
                          instrument = c("Equity A", 
                                         "Equity B"),
                          .Names = c("Equity A", "Equity B")))
                
    
    
    checkEquals(as.data.frame(x),
                structure(list(pl = c(2, 60),
                               buy = c(100, 4),
                               sell = c(102, 7),
                               volume = c(2, 40)),
                          .Names = c("pl", "buy",
                                     "sell", "volume"),
                          row.names = c("Equity A", "Equity B"),
                          class = "data.frame"))

    ## open positions and no eval.price specified => pl is NA
    timestamp <- 1:4
    amount <- c(1,1,1,1)
    price <- 101:104
    instrument <- "Bond"
    jnl <- journal(timestamp=timestamp, amount=amount,
                   price=price, instrument=instrument)
    checkTrue(is.na(suppressWarnings(pl(jnl)[[1]][["pl"]])))



    ## initial position
    pl(journal(),
       initial.position = 1,
       initial.price = 100,
       eval.price = 105)

    pl(journal(),
       initial.position = c(A = 1, B = 2),
       initial.price = c(A = 100),
       eval.price = c(A = 105, B = 110))

    pl(journal(),
       initial.position = c(A = 1, B = 2),
       initial.price = c(A = 100, B = 100),
       eval.price = c(A = 105, B = 110))

    pl(journal(), multiplier = 2,
       initial.position = c(A = 1, B = 2),
       initial.price = c(A = 100, B = 100),
       eval.price = c(A = 105, B = 110))

    1
    
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



## pl(amount = 1, price = 1, eval.price = 2)

## pl(amount = 1, price = 2, initial.position = 1,
##    initial.price = 1, eval.price = 3)


## amount <- c(1,1,-1,1,-2)
## price <- c(100,102,105,102,105)

## cumcash <- cumsum(-price * amount)
## cumpos  <- cumsum(amount)
## pnl <- cumpos * price + cumcash
## real <- avg(amount, price)$realised
## unreal <- pnl-real
## data.frame(cumsum(amount), price, pnl, real, unreal)


    ## test multiplier
    require("PMwR")
    require("RUnit")
    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2),
                   multiplier = 0)[[1L]][["pl"]], 0)
    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2),
                   multiplier = 1)[[1L]][["pl"]], 1)
    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2))[[1L]][["pl"]], 1)
    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2),
                   multiplier = 2)[[1]][["pl"]], 2)

    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2),
                   instrument = c("B", "B"),
                   multiplier = c(A = 1, B = 2))[[1L]][["pl"]], 2)

    
}

## pl(amount = 1, price = 1,
##    initial.position = 1, initial.price = 1,
##    eval.price = 2)


## instrument  <- c("FGBL", "FGBL", "Bond", "Bond")
## amount <- c(1, -2, 2, -1)
## price <- c(100,101, 1, 5)
## ## .pl(amount, price)
## pl.default(amount, price, instrument=instrument)
## pl.default(amount, price, instrument=instrument, eval.price=c(FGBL=103, Bond = 2))

## amount <- c(1, -2)
## price <- c(100,101)
## pl.default(amount, price)
## pl.default(amount, price, eval.price=100)

## require("rbenchmark")

## amount <- rep(c(1,-1), times = 1000)
## price <- rep(c(100,101), times = 1000)

## benchmark(-drop(crossprod(amount, price)),
##           -c(crossprod(amount, price)),
##           -sum(amount * price),
##           columns = c("test", "elapsed", "relative"),
##           replications = 100000, order ="relative")
