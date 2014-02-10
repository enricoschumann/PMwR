## -*- truncate-lines: t; -*-
## Time-stamp: <2014-02-10 15:32:19 CET (es)>
test.btest <- function() {

    require("PMwR")
    require("RUnit")

    btTable <- function(solution, prices)
        data.frame(prices = prices,
                   position     = solution$position,
                   suggested    = solution$suggested.position,
                   wealth = solution$wealth,
                   cash   = solution$cash)

    prices <- c(100,98,98,97,101,102,101,98,99,101)
    
    ## signal returns NULL: not trade at all
    signal <- function()
        NULL
    solution <- btest(prices = prices, signal = signal)
    checkEquals(drop(solution$position), rep(0, length(prices)))
    checkEquals(drop(solution$wealth), rep(0, length(prices)))
    checkEquals(solution$journal, journal())

    ## ... initial wealth not zero
    solution <- btest(prices = prices, signal = signal, initial.cash = 100)
    checkEquals(drop(solution$position), rep(0, length(prices)))
    checkEquals(drop(solution$wealth), rep(100, length(prices)))
    checkEquals(solution$journal, journal())

    ## ... initial position not zero
    solution <- btest(prices = prices, signal = signal, initial.position = 2)
    checkEquals(drop(solution$position), rep(2, length(prices)))
    checkEquals(drop(solution$wealth), prices*2)
    checkEquals(solution$journal, journal())

    ## signal returns 1: hold one unit of asset
    signal <- function()
        1

    ## ... default settings
    solution <- btest(prices = prices, signal = signal)
    checkEquals(solution$position,
                structure(c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(length(prices), 1L)))
    checkEquals(solution$wealth,
                c(0, 0, 0, -1, 3, 4, 3, 0, 1, 3))

    ## ... with no burnin
    solution <- btest(prices = prices, signal = signal, b = 0)
    checkEquals(solution$position,
                structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(length(prices), 1L)))
    checkEquals(solution$wealth,
                c(0, -2, -2, -3, 1, 2, 1, -2, -1, 1))

    ## signal returns a weight
    signal <- function()
        0.12

    ## ... no position (since wealth is 0)
    solution <- btest(prices = prices, signal = signal, convert.weights = TRUE)
    checkEquals(drop(solution$position), rep(0, length(prices)))
    checkEquals(drop(solution$wealth), rep(0, length(prices)))
    checkEquals(drop(solution$suggested.position), rep(0, length(prices)))
    checkEquals(solution$journal, journal())
    

    solution <- btest(prices = prices, signal = signal, convert.weights = TRUE,
                      initial.cash = 1000)
    checkEquals((solution$wealth * signal()/prices)[-length(prices)],
                solution$suggested.position[-1]) 
    checkEquals((solution$wealth * signal()/prices)[-length(prices)],
                solution$position[-1]) 


    ## signal returns a weight, 2 assets
    prices2 <- cbind(A = prices, B = prices/2)
    signal <- function()
        c(0.2, 0.3)

    ## ... no initial wealth, no position
    solution <- btest(list(prices2), signal = signal, convert.weights = TRUE)
    checkEquals(dim(solution$position), dim(prices2))
    checkTrue(all(solution$position==0))
    checkEquals(solution$journal, journal())

    ## ... with initial wealth
    solution <- btest(list(prices2), signal = signal, convert.weights = TRUE,
                      initial.cash = 1000)
    checkEquals((outer(solution$wealth, signal())/prices2)[-nrow(prices2), ],
                solution$position[-1L, ])

    ## ... with rebalancing in only 2 period
    do.rebalance <- function()
        if (Time() == 3L || Time() == 8L)
            TRUE else FALSE
    solution <- btest(list(prices2), signal = signal, convert.weights = TRUE,
                      initial.cash = 1000, do.rebalance = do.rebalance)
    checkEquals(solution$position,
                structure(c(0, 0, 0,
                            2.04081632653061, 2.04081632653061,
                            2.04081632653061, 2.04081632653061,
                            2.04081632653061, 2.0512286547272,
                            2.0512286547272, 0, 0, 0,
                            6.12244897959184,
                            6.12244897959184, 6.12244897959184, 
                            6.12244897959184, 6.12244897959184,
                            6.15368596418159, 6.15368596418159),
                          .Dim = c(10L, 2L), .Dimnames = list(NULL, c("A", "B"))))
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[4,]))
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[5,]))
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[6,]))
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[7,]))
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[8,]))
    checkEquals(c(signal()*solution$wealth[8]/prices2[8,]),
                c(solution$position[9,]))
    checkEquals(c(signal()*solution$wealth[8]/prices2[8,]),
                c(solution$position[10,]))
    
    ## signal returns a weight, 3 assets
    prices3 <- cbind(A = prices, B = prices/2, C = prices/3)
    signal <- function()
        c(0.2, 0.3, 0.25)

    ## ... no initial wealth, no position
    solution <- btest(list(prices3), signal = signal, convert.weights = TRUE)
    checkEquals(dim(solution$position), dim(prices3))
    checkTrue(all(solution$position==0))
    checkEquals(solution$journal, journal())

    ## ... no initial wealth, no position
    solution <- btest(list(prices3), signal = signal, convert.weights = TRUE,
                      initial.cash = 1000)
    checkEquals((outer(solution$wealth, signal())/prices3)[-nrow(prices3), ],
                solution$position[-1L, ])

}
