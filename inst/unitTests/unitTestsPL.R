
test.PL <- function() {

    ## case 1
    times <- c(3,1,4); notional <- c(-1,2,-1); prices <- c(102,100,103)
    res <- PL(notional, prices)
    res0 <- structure(list(averagePriceBuy = 100,
                           averagePriceSell = 102.5,
                           PLtotal = 5, PLperContract = 2.5,
                           absNotional = 4),
                      .Names = c("averagePriceBuy", "averagePriceSell",
                      "PLtotal", "PLperContract", "absNotional"))
    checkEquals(res,res0)
    checkEquals(tail(PLsorted(notional, prices)$wealth, 1L), res$PLtotal)

    ## case 2: buy in fist period and hold
    allprices <- 100:105
    alltimes  <- 1:6
    times <- 1; notional <- 2; prices <- 100
    PLsorted(notional, prices, tradetimes = times,
             alltimes = alltimes, allprices = allprices)


    ## case 3: buy in last period
    allprices <- 100:105
    alltimes  <- 1:6
    times <- 6; notional <- 2; prices <- 100
    PLsorted(notional, prices, tradetimes = times,
             alltimes = alltimes, allprices = allprices)

    ## case 4: buy several times in first period
    allprices <- 100:105
    alltimes  <- 1:6
    times <- c(1,1); notional <- c(1,1); prices <- c(100.2, 100.5)
    PLsorted(notional, prices, tradetimes = times,
             alltimes = alltimes, allprices = allprices)

    ## case 5: buy several times in one period
    allprices <- 100:105
    alltimes  <- 1:6
    times <- c(1,1,2,2); notional <- c(1,1,-1,-1)
    prices <- c(100.2, 100.5, 101,102)
    PLsorted(notional, prices, tradetimes = times,
             alltimes = alltimes, allprices = allprices)
    checkEquals(tail(PLsorted(notional, prices)$wealth, 1L),
                PL(notional, prices)$PLtotal)

    ## case 6: trade in one period
    allprices <- 100:105
    alltimes  <- 1:6
    times <- c(1,1); notional <- c(1,-1)
    prices <- c(100, 101)
    PL(notional, prices)
    checkEquals(tail(PLsorted(notional, prices)$wealth, 1L),
                PL(notional, prices)$PLtotal)
    checkEquals(tail(PLsorted(notional, prices,
                              tradetimes = times,
                              alltimes = alltimes,
                              allprices = allprices)$wealth, 1L),
                PL(notional, prices)$PLtotal)

    ## case 7: trade in one period
    allprices <- 100:105
    alltimes  <- 1:6
    times <- c(1,1,2,3); notional <- c(1,-1,1,-1)
    prices <- c(100, 101, 101,102)
    PLsorted(notional, prices, tradetimes = times,
             alltimes = alltimes, allprices = allprices)
    PL(notional, prices)
    checkEquals(tail(PLsorted(notional, prices)$wealth, 1L),
                PL(notional, prices)$PLtotal)

    ## case 8: trade in one period, with fractions
    allprices <- 100:105
    alltimes  <- 1:6
    times <- c(1, 1, 1, 1, 3); notional <- c(-0.2, -0.1, 1, 0.4, -1.1)
    prices <- c(101, 101, 101, 101,102)
    PLsorted(notional, prices, tradetimes = times,
             alltimes = alltimes, allprices = allprices)
    PL(notional, prices)
    checkEquals(tail(PLsorted(notional, prices)$wealth, 1L),
                PL(notional, prices)$PLtotal)
    checkEquals(tail(PLsorted(notional, prices,tradetimes = times,
                              alltimes = alltimes,
                              allprices = allprices)$wealth, 1L),
                PL(notional, prices)$PLtotal)

    ## case 9: trade in one period, with fractions
    allprices <- 100:105
    alltimes  <- 1:6
    times <- c(1, 1, 1, 1, 3)
    prices <- c(101, 101, 101, 101,102)
    notional <- c(-0.2, 0.2, 0.2,0.2, -0.4)
    checkEquals(tail(PLsorted(notional, prices, tradetimes = times,
                              alltimes = alltimes,
                              allprices = allprices)$wealth,
                     1L),
                PL(notional, prices)$PLtotal)

    notional <- c(0.2, -0.2, -0.2,-0.2, 0.4)
    checkEquals(tail(PLsorted(notional, prices, tradetimes = times,
                              alltimes = alltimes,
                              allprices = allprices)$wealth,
                     1L),
                PL(notional, prices)$PLtotal)


    ## actual case
    times <- c(20120912091902, 20120912093054,
               20120912100552, 20120912100552,
               20120912100553, 20120912102031)

    n <- c(-5L, -5L, -5L, 5L, 5L, 5L)
    p <- c(140.12, 140.1, 140.28, 140.42, 140.31, 139.58)
    checkEquals(tail(PLsorted(n, p,
                              tradetimes = times,
                              alltimes  = times,
                              allprices = p)$wealth, 1L),
                PL(n,p)$PLtotal)






    if (FALSE) {
        allprices <- 100:105
        alltimes  <- 1:6
        PLsorted(notional, prices, tradetimes = times,
                 alltimes = alltimes, allprices = allprices)
        PLsorted(notional, prices, tradetimes = times,
                 alltimes = alltimes, allprices = allprices,
                 do.sort = TRUE)
        PLsorted(notional, prices, tradetimes = times,
                 alltimes = alltimes, allprices = allprices,
                 initcash = 200,
                 do.sort = TRUE)



        allprices <- c(100,101,102,100,101)
        alltimes  <- c(1,3,4,5,6)
        notional <- c(1,-1)
        prices <- c(99,103)
        tradetimes <- c(2,3)

        res <- PLsorted(notional, prices, tradetimes,
                        allprices, alltimes,
                        initcash=0, FALSE)
        data.frame(res)

        notional <- c(-1,1)
        prices <- c(103,99)
        tradetimes <- c(3,2)
        res <- PLsorted(notional, prices, tradetimes,
                        allprices, alltimes,
                        initcash=0, FALSE)
        data.frame(res)


        ## returns
        t <- 100:105
        all.equal(returns(t),
                  c(0.01, 0.00990099009900991, 0.00980392156862742,
                    0.00970873786407767, 0.00961538461538458))

    }
}
