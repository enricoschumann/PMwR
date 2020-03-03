library("zoo")
t <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "1 day")
x <- seq_along(t)/10 + 100
z <- zoo(x, t)
txt <- capture.output(returns(z, period = "month"))
expect_equal("2012 3.0 2.8 2.9 2.7 2.8 2.6 2.6 2.6 2.4 2.4 2.3 2.3 36.5",
             txt[2])
expect_equal(length(txt), 2)

expect_true(inherits(returns(x, t = t, period = "month"), "p_returns"))
expect_true(inherits(returns(x, t = t, period = "month"), "p_returns_monthly"))

expect_true(inherits(returns(z,        period = "month"), "p_returns"))
expect_true(inherits(returns(z,        period = "month"), "p_returns_monthly"))
