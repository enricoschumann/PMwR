prices <- 1:5
timestamp <- Sys.Date() + 0:4

res <- btest(prices = 1:5,
             signal = function() 1,
             timestamp = timestamp,
             b = timestamp[1L] + 0.5)
expect_equal(res$b, 1)

res <- btest(prices = 1:5,
             signal = function() 1,
             timestamp = timestamp,
             b = timestamp[1L])
expect_equal(res$b, 1)

res <- btest(prices = 1:5,
             signal = function() 1,
             timestamp = timestamp,
             b = timestamp[1L] - 0.5)
expect_equal(res$b, 0)
