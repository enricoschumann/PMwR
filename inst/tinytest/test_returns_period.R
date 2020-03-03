## yearly returns
library("datetimeutils", quietly = TRUE)

dates <- sort(as.Date("1990-01-01") + sample(3700, 1000))
x <- seq_along(dates)
R <- returns(x, t = dates, period = "year")

n <- nth_day(dates, period = "year", n = "last")
ni <- nth_day(dates, period = "year", n = "last", index = TRUE)
if (!1 %in% ni)
    ni <- c(1, ni)
expect_equivalent(.returns(x[ni], lag = 1), unclass(R))
expect_equal(attr(R, "t"), n)

R <- returns(x, t = dates, period = "year", complete.first = FALSE)
n <- nth_day(dates, period = "year", n = "last")
ni <- nth_day(dates, period = "year", n = "last", index = TRUE)
expect_equivalent(.returns(x[ni], lag = 1), unclass(R))
expect_equal(attr(R, "t"), n[-1])

## itd: zero and NA returns
expect_equivalent(unclass   (returns(rep(1, 10), period = "itd")), 0)
expect_equivalent(as.numeric(returns(rep(1, 10), period = "itd")), 0)

expect_equivalent(unclass   (returns(rep(NA, 10), period = "itd")), NA_real_)
expect_equivalent(as.numeric(returns(rep(NA, 10), period = "itd")), NA_real_)

## ... preferred alias for 'itd': total
expect_equivalent(unclass   (returns(rep(1, 10), period = "total")), 0)
expect_equivalent(as.numeric(returns(rep(1, 10), period = "total")), 0)

expect_equivalent(unclass   (returns(rep(NA, 10), period = "total")), NA_real_)
expect_equivalent(as.numeric(returns(rep(NA, 10), period = "total")), NA_real_)
