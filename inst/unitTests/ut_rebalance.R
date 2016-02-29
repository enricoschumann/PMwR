test.rebalance <- function() {

    require("RUnit")
    require("PMwR")
    current <- c(0,0,100,100)
    prices  <- c(1,1,1,1)
    target  <- c(0.25, 0.25, 0.25, 0.25)
    x <- rebalance(current, target, prices, match.names = FALSE)
    checkEquals(x$target, rep(50, 4))

    ## no initial position: 'current' is 0
    current <- 0
    prices  <- c(1,1,1,2)
    target  <- c(0.25, 0.25, 0.25, 0.25)
    x <- rebalance(current, target, prices, match.names = FALSE, notional = 100)
    checkEquals(x$target, c(rep(25,3), 12))
    
    ## liquidate all: 'target' is 0
    current <- c(0,0,100,100)
    x <- rebalance(current, target = 0, prices, match.names = FALSE, notional = 100)
    checkEquals(x$target, rep(0,4))

    current <- c(0,0,-100,-100)
    x <- rebalance(current, target = 0, prices, match.names = FALSE, notional = 100)
    checkEquals(x$target, rep(0,4))

    ## *names*
    prices  <- c(1,1,1,1)
    names(prices) <- letters[1:4]

    current <- c(b = 10)
    target  <- c(d = 0.1)

    x <- rebalance(current, target, prices, match.names = TRUE)
    checkEquals(x$target, c(d=1))

    ## *journal*
    j <- journal(amount = c(1, 2),
                 instrument = c("A", "B"))
    w <- c(A = 0.5, B = 0.5)
    
    amount <- rebalance(position(j), w, price = c(A = 1, B = 12))
    checkEquals(journal(amount),
                structure(list(instrument = c("A", "B"),
                               timestamp = c(NA, NA),
                               amount = c(11, -1),
                               price = c(1, 12)),
                          .Names = c("instrument", "timestamp",
                                     "amount", "price"),
                          class = "journal"))

    
    checkEquals(journal(amount, price = FALSE),
                structure(list(instrument = c("A", "B"),
                               timestamp  = c(NA, NA),
                               amount     = c(11, -1),
                               price      = c(NA, NA)),
                          .Names = c("instrument", "timestamp",
                                     "amount", "price"),
                          class = "journal"))    



## price <- c(a = 1, b = 2, c = 3)
## current <- c(a = 100, b = 20)
## target <- c(a = 0.2, c = 0.3)
## rebalance(current, target, price)

## price <- c(1,2,3)
## current <- c(100, 20, 0)
## target <- c(0.2, 0, 0.3)
## rebalance(current, target, price, match.names = FALSE)

## require("PMwR")

## j <- journal(amount = c(1, 2),
##              instrument = c("A", "B"),
##              price = c(1, 10))


## w <- c(A = 0.5, B = 0.5)
## (x <- rebalance(position(j), target=w, price = c(A=2, B =12)))
## journal(x)


## ins <- attr(position(j), "instrument")

## pos <- as.numeric(position(j))
## names(pos) <- attr(position(j), "instrument")

## rebalance(pos, target=w, price = c(A=2, B =12))

## j <- journal(amount = c(1, 2),
##              price = c(1, 10))
## w <- c(0.5)
## amount <- rebalance(position(j), w, price = 1, match.names=FALSE)

## j <- journal(amount = c(1, 2),
##              instrument = c("A", "B"),
##              price = c(1, 10))
## w <- c(A = 0.5, B = 0.5)

## amount <- rebalance(position(j), w, price = c(A=1, B=12))
## journal(amount)
## dput(journal(amount))

}
