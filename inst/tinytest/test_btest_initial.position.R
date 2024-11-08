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


## -------------------------------------------

P <- 100:110
bt0 <- btest(P,
             initial.position = 1,
             b = 0,
             do.signal = FALSE)
bt1 <- btest(P,
             initial.position = 1,
             b = 1,
             do.signal = FALSE)
expect_equal(bt0$wealth, bt1$wealth)
expect_equal(bt0$wealth, 100:110)


## -------------------------------------------

P <- 100:110
P <- cbind(P, NA)
bt <- btest(list(P),
            initial.position = c(1, 0),
            b = 0,
            do.signal = FALSE)
expect_equal(bt$wealth, 100:110)

bt <- btest(list(P),
            initial.position = c(1, 0),
            b = 0,
            signal = function() NULL)
expect_equal(bt$wealth, 100:110)

debug(btest)
bt <- btest(list(P),
            initial.position = c(1, 0),
            b = 0,
            do.signal = function() TRUE,
            signal = function() c(0.5,0))

expect_equal(bt$wealth,
             c(100.0, 100.5, 101.0, 101.5, 102, 102.5,
               103.0, 103.5, 104.0, 104.5, 105))
expect_equal(bt$cash,
             rep(50, nrow(P)))
