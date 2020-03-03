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
