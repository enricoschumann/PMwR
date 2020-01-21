## replications

prices <- 1:10
ans1 <- btest(prices, function() 1)
expect_true(inherits(ans1, "btest"))

## ans <- btest(prices, function() 1, replications = 1)
## expect_equal(length(ans), 1)
## tmp <- ans[[1]]
## attr(tmp, "replication") <- NULL
## expect_equal(tmp, ans1)

## ans <- btest(prices, function() 1, replications = 2)
## expect_equal(length(ans), 2)
## tmp <- ans[[1]]
## attr(tmp, "replication") <- NULL
## expect_equal(tmp, ans1)
## tmp <- ans[[2]]
## attr(tmp, "replication") <- NULL
## expect_equal(tmp, ans1)
