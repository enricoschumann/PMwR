
nav <- NAVseries(1:10)
expect_equal(c(nav), 1:10)
expect_equal(attr(nav, "timestamp"), 1:10)  ## integer timestamp added
## summary
sum.nav <- summary(nav)[[1]]
expect_equal(sum.nav$return, tail(nav,1)/head(nav, 1)-1)
expect_equal(sum.nav$nna, 0)
expect_equal(sum.nav$nobs, 10)



## with NA
nav <- NAVseries(c(1, NA, 3:10))
expect_equal(attr(nav, "timestamp"), 1:10)  ## integer timestamp added
## summary
sum.nav <- summary(nav)[[1]]
expect_equal(sum.nav$return, tail(nav,1)/head(nav, 1)-1)
expect_equal(sum.nav$nna, 1)
expect_equal(sum.nav$nobs, 10)


## with Date timestamp
nav <- NAVseries(1:10, timestamp = as.Date("2017-1-1")+0:9)
expect_equal(c(nav), 1:10)
expect_equal(attr(nav, "timestamp"), as.Date("2017-1-1")+0:9)
## summary
sum.nav <- summary(nav)[[1]]
expect_equal(sum.nav$return, tail(nav,1)/head(nav, 1)-1)
expect_equal(sum.nav$nna, 0)
expect_equal(sum.nav$nobs, 10)

## scale1
expect_equivalent(scale1(NAVseries(10:15), level = 100),
                  NAVseries((10:15)*10))


prices <- 100:109

signal <- function()
    1
bt <- btest(prices = prices, signal = signal, b = 0,
            initial.cash = 100)
expect_equal(c(as.NAVseries(bt)), 100:109)

## summary(nav, monthly = FALSE)

## plot(NAVseries(1:10))
## plot(NAVseries(1:10, timestamp = as.Date("2017-1-1")+0:9))

expect_equal(returns(as.NAVseries(bt)),
             returns(prices))


## leading NAs
bt <- btest(prices = prices, signal = signal,
            b = 2, initial.cash = 100)

### ... no NAs
expect_equivalent(as.numeric(as.NAVseries(bt)),
                  c(100, 100:107))

### ... one NA
expect_equivalent(as.numeric(as.NAVseries(bt, drop.NA = FALSE)),
                  c(NA, 100, 100:107))

### ... several NAs
bt <- btest(prices = prices, signal = signal,
            b = 5, initial.cash = 100)
expect_equivalent(as.numeric(as.NAVseries(bt, drop.NA = FALSE)),
                  c(NA, NA, NA, NA, 100, 100, 101, 102, 103, 104))
