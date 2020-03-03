x <- 1:10
expect_equal(returns(x),
             returns(x, lag = 1))

expect_equal(returns(x, lag = 2),
             x[3:10]/x[1:8] - 1)

library("zoo")
t <- as.Date("2000-1-1")+1:10
expect_equal(returns(x, t, lag = 2),  ## 't' is ignored
             returns(x, lag = 2))
expect_equivalent(unclass(returns(zoo(x, t), lag = 2)),
                  returns(x, lag = 2))

expect_equal(index(returns(zoo(x, t), lag = 2)), t[-c(1:2)])

expect_equal(index(returns(zoo(x, t), lag = 2, pad = NA)), t)

expect_true(all(is.na(
    coredata(returns(zoo(x, t), lag = 2, pad = NA))[1:2])))

