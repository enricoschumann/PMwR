test.pl <- function() {

    require("RUnit")
    require("PMwR")
    current <- c(0,0,100,100)
    prices  <- c(1,1,1,1)
    target  <- c(0.25, 0.25, 0.25, 0.25)
    x <- rebalance(current, target, prices, match.names = FALSE)
    checkEquals(x$target, rep(50,4))

    ## no initial position
    current <- 0
    prices  <- c(1,1,1,2)
    target  <- c(0.25, 0.25, 0.25, 0.25)
    x <- rebalance(current, target, prices, match.names = FALSE, notional = 100)
    checkEquals(x$target, c(rep(25,3), 12))
    
    ## liquidate all
    current <- c(0,0,100,100)
    x <- rebalance(current, target, prices, match.names = FALSE, notional = 100)
    checkEquals(x$target, rep(0,4))

    current <- c(0,0,-100,-100)
    x <- rebalance(current, target, prices, match.names = FALSE, notional = 100)
    checkEquals(x$target, rep(0,4))


    ## *names*
    prices  <- c(1,1,1,1)
    names(prices) <- letters[1:4]

    current <- c(b = 10)
    target  <- c(d = 0.1)

    x <- rebalance(current, target, prices, match.names = FALSE)
    checkEquals(x$target, rep(50,4))
    
}
