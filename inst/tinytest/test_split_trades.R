amount <- c(1, -1)
price <- c(1, 2)
ans <- split_trades(amount, price, seq_along(amount))
expect_equal(length(ans), 1)
expect_equal(ans,
            list(structure(list(amount = c(1, -1),
                                price = c(1, 2),
                                timestamp = 1:2),
                           .Names = c("amount", "price", "timestamp"))))



amount <- c(1, -2, 1)
price <- c(1,2,3)
ans <- split_trades(amount, price, seq_along(amount))
expect_equal(length(ans), 2)

expect_equal(ans[[1L]],
            structure(list(amount = c(1, -1),
                           price = c(1, 2),
                           timestamp = 1:2),
                      .Names = c("amount", "price", "timestamp")))
expect_equal(ans[[2L]],
            structure(list(amount = c(-1, 1),
                           price = c(2, 3),
                           timestamp = 2:3),
                      .Names = c("amount", "price", "timestamp")))

n <- c(1,1,-3,1)
p <- c(1,2,3,2)
tradetimes <- seq_along(n)
ans <- split_trades(n, p, tradetimes)
expect_equal(length(ans), 2)



## https://quant.stackexchange.com/questions/75751/what-are-the-packages-for-effective-backtesting-in-r/75762#75762
P <- c(100, 99, 104, 103, 105, 104)  ## price series
S <- c(  0,  1,   1,   0,   1,   0)  ## position to be held
dS <- c(0, diff(S)) ## change in position ==> trades
split_trades(amount = dS, price = P)


P <- c(100, 99, 104, 103, 102, 105, 104)  ## price series
S <- c(  0,  1,   1,   0,   0,   1,   0)  ## position to be held
dS <- c(0, diff(S)) ## change in position ==> trades
expect_equal(length(sp <- split_trades(amount = dS, price = P)),
             4L)
expect_equivalent(sum(
    sapply(sp, function(x) pl(as.journal(x), pl.only = TRUE))),
    3)

expect_equal(length(sp <- split_trades(amount = dS,
                                       price = P,
                                       drop.zero = TRUE)),
             2L)
expect_equivalent(
    sum(sapply(sp, function(x) pl(as.journal(x), pl.only = TRUE))),
    3)


P <- c(100, 99, 104, 103, 101, 102, 105, 104)  ## price series
S <- c(  0,  1,   1,   0,   0,   0,   1,   0)  ## position to be held
dS <- c(0, diff(S)) ## change in position ==> trades
expect_equal(length(split_trades(amount = dS, price = P,
                                 drop.zero = TRUE)),
             2L)
