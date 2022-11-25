prices <- 1:10
expect_equal(
    length(journal(btest(prices = prices,
                         signal = function() NULL,
                         initial.position = 1,
                         prices0 = 1,
                         b = 0))),
    0) ## => there should be no trade at all


## -------------------------------------------


## no trade, but initial position exists
## and is held and valued.
P <- 100:110
bt <- btest(P,
            initial.position = 1,
            ## signal = function() NULL,
            do.signal = FALSE)
expect_equal(bt$wealth, 100:110)



P <- cbind(100:110, 200:210)
bt <- btest(list(P),
            initial.position = c(1, 2),
            ## signal = function() NULL,
            do.signal = FALSE)
expect_equivalent(bt$wealth, c(P %*% c(1, 2)))

P <- cbind(100:110, 200:210)
bt <- btest(list(P),
            initial.position = c(1, 2),
            prices0 = c(1, 2),            ## with prices
            ## signal = function() NULL,
            do.signal = FALSE)
expect_equivalent(bt$wealth, c(P %*% c(1, 2)))

P <- cbind(100:110, 200:210)
bt <- btest(list(P),
            initial.position  = c(1, 2),
            prices0 = t(c(1, 2)),         ## with prices
            signal = function() c(1, 2),  ##      and signal
            b = 0,
            do.signal = TRUE)
expect_equivalent(bt$wealth, c(P %*% c(1, 2)))
