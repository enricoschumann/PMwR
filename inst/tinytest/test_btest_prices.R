prices <- 1:5
expect_equal(
    btest(prices, signal = function() 1),
    btest(as.matrix(prices), signal = function() 1))

expect_equal(
    btest(prices, signal = function() 1),
    btest(list(prices), signal = function() 1))

expect_equal(
    btest(prices, signal = function() 1),
    btest(list(as.matrix(prices)), signal = function() 1))

library("zoo")
bt1 <- btest(prices, signal = function() 1)
bt2 <- btest(zoo(prices), signal = function() 1)
expect_equivalent(bt1$wealth, bt2$wealth)
expect_equivalent(bt1$position, bt2$position)
expect_equivalent(bt1$suggested.position,
                   bt2$suggested.position)
