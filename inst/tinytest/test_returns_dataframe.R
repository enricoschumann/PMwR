## returns without 'period'

x <- 1:10
expect_true(inherits(returns(data.frame(x)), "data.frame"))
expect_equal(returns(data.frame(x))[[1]], returns(x))
expect_equal(returns(data.frame(x), pad = 0)[[1]], returns(x, pad = 0))


x <- cbind(1:10, 2:11)
expect_true(inherits(returns(data.frame(x)), "data.frame"))
expect_equivalent(as.matrix(returns(data.frame(x))), returns(x))
expect_equivalent(as.matrix(returns(data.frame(x), pad = 0)), returns(x, pad = 0))


## returns with 'period'
x <- DAX[[1]]
t <- as.Date(row.names(DAX))
returns(x, t, period = "ytd")
returns(data.frame(x), t, period = "ytd")

## expect_true(inherits(returns(data.frame(x), t, period = "quarter"), "data.frame"))
## expect_true(inherits(returns(data.frame(cbind(x, x)), t, period = "quarter"), "data.frame"))
