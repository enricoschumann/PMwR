## -*- truncate-lines: t; -*-
## Time-stamp: <2013-09-16 15:12:33 CEST (es)>
test.btest <- function() {

    require("PMwR")
    require("RUnit")


    prices <- c(100,98,98,97,101,102,101,98,99,101)
    position <- function()
        1
    solution <- btest(prices = prices,
                      position = position)

    btTable <- function(solution, prices)
        data.frame(prices = prices,
                   position     = solution$position,
                   suggested    = solution$suggested.position,
                   wealth = solution$wealth,
                   cash   = solution$cash)
    
    btTable(solution, prices)
    solution$journal
}
