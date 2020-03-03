
library("zoo", quietly = TRUE, warn.conflicts = FALSE)

## numeric vector
x <- 101:112
expect_equivalent(returns(x), x[-1]/x[-length(x)]-1)
expect_equivalent(returns(x, pad = NA)[-1], x[-1]/x[-length(x)]-1)
expect_true(is.na(returns(x, pad = NA)[1]))


## numeric matrix
x <- cbind(x,x)
expect_equivalent(returns(x), x[-1,]/x[-nrow(x),] - 1)
expect_equivalent(returns(x, pad = NA)[-1,], x[-1, ]/x[-nrow(x),]-1)
expect_true(all(is.na(returns(x, pad = NA)[1L,])))


## data.frame
y <- as.data.frame(x)
expect_true(inherits(returns(y), "data.frame"))
expect_equal(returns(y), y[-1,]/y[-nrow(x),] - 1)
expect_equivalent(as.matrix(returns(y, pad = NA)[-1,]), x[-1, ]/x[-nrow(x),]-1)
expect_true(all(is.na(returns(y, pad = NA)[1L,])))
row.names(y) <- letters[1:nrow(y)]
expect_equal(returns(y), y[-1,]/y[-nrow(x),] - 1)
expect_equal(returns(x, pad = NA)[-1,], x[-1, ]/x[-nrow(x),]-1)


## lagged returns -- numeric vector
x <- 101:112; lag <- 4
expect_equivalent(returns(x, lag = lag),
                  x[-(1:lag)]/x[-((length(x)-lag+1):length(x))]-1)
expect_equivalent(returns(x, pad = NA, lag = lag)[-(1:lag)],
                  x[-(1:lag)]/x[-((length(x)-lag+1):length(x))]-1)
expect_true(all(is.na(returns(x, pad = NA, lag = lag)[1:lag])))


## lagged returns -- matrix
x <- cbind(x,x)
expect_equivalent(returns(x, lag = lag),
                  x[-(1:lag), ]/x[-((nrow(x)-lag+1):nrow(x)), ] - 1)
expect_equivalent(returns(x, pad = NA, lag = lag)[-(1:lag),],
                  x[-(1:lag), ]/x[-((nrow(x)-lag+1):nrow(x)), ] - 1)
expect_true(all(is.na(returns(x, pad = NA, lag = lag)[1:lag, ])))


## zoo -- numeric vector
x <- 101:112
z <- zoo(x, seq_along(x))
expect_equal(returns(as.numeric(z)), returns(x))
expect_equal(returns(as.numeric(z), pad = 0), returns(x, pad = 0))

expect_equal(returns(z),
             zoo(returns(as.numeric(z)), index(z)[-1]))
expect_equal(returns(z, pad = 0),
             zoo(returns(as.numeric(z), pad = 0), index(z)))


## padding in zoo -- numeric vector
expect_true(is.na(returns(z, pad = NA)[1L]))
expect_true(coredata(returns(z, pad = 0)[1L]) == 0)
expect_true(coredata(returns(z, pad = 1)[1L]) == 1)


## period, but no timestamp: period is ignored.
## timestamp, but no period: timestamp is ignored.
##
## (when there is no period/rebalance.when, methods
## are required to keep timestamp information for
## themselves and then to re-assemble the necessary
## class structure)
x <- 101:112
t <- seq_along(x)
suppressWarnings(expect_equal(returns(x, period = "month"), returns(x)))
suppressWarnings(expect_equal(returns(x, t = t),            returns(x)))

## period -- check class
t <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "1 day")
x <- seq_along(t)/10 + 100
z <- zoo(x, t)
## z <- cbind(z,z,z)
returns(z, period = "mtd")

expect_true("p_returns" %in% class(returns(x, t = t, period = "month")))
expect_true("p_returns" %in% class(returns(z,        period = "month")))
expect_true(class(returns(z)) == "zoo")

## period -- zoo or specify t
expect_equal(returns(x, t = t, period = "month"),
             returns(z,        period = "month"))
expect_equal(returns(x, t = t, period = "month", pad = NA),
             returns(z,        period = "month", pad = NA))

## as.zoo for p_returns
ti <- match(aggregate(t, by = list(format(t, "%Y%m")), FUN = tail, 1)[[2]], t)
expect_equal(as.zoo(returns(x, t = t, period = "month")),
             zoo(returns(x[c(1, ti)]), t[c(ti)]))

## period -- month end
expect_equal(c(returns(x, t = t, period = "month", complete.first = FALSE)),
             returns(x[ti]))
expect_equal(c(returns(x, t = t, period = "month", complete.first = TRUE)),
             returns(x[c(1,ti)]))
expect_equal(c(returns(x, t = t, period = "month")),
             returns(x[c(1,ti)]))


## period -- ytd
## --> supress warning that 2012 is not the current year
expect_equal(c(suppressWarnings(returns(x, t = t, period = "ytd"))),
             tail(x,1)/head(x,1) - 1)

## period -- mtd
expect_equal(c(returns(x, t = t, period = "mtd")),
             tail(x, 1) / x[match(as.Date("2012-11-30"),t)] - 1)

## period -- quarterly
expect_equal(c(returns(x, t = t, period = "quarterly")),
             returns(x[match(as.Date(c("2012-1-1",
                                       "2012-3-31",
                                       "2012-6-30",
                                       "2012-9-30",
                                       "2012-12-31")),t)]))



## from journal to time-weighted returns

prices <- cbind(a = 101:110, b = 201:210)

j <- journal(timestamp  = c(1,4,4,5,5,7),
             amount     = c(1,1,1,-1,1,-1),
             instrument = c("a", "a", "b", "a", "b", "a"),
             price      = c(100.5,104.1,203,105,205.2,108))

p <- position(j, when = 1:10)
rowSums(p*prices)


## missing values
x <- zoo(c(NA, 2:5), as.Date("2017-10-27") + 1:5)
expect_equivalent(unclass(returns(x, period = "month")), c(NA, 0.25))
