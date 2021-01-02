library("PMwR")
library("tinytest")
prices <- cbind(a = 101:105, b = 201:205)

## 2 assets, with sum(weights) == 1
weights <- c(0.8, 0.2)

### => no rebalancing at all
ans <- returns(prices, weights = weights,
               rebalance.when = FALSE)
expect_equivalent(ans, rep(0, nrow(prices) - 1))

ans <- returns(prices, weights = weights,
               rebalance.when = FALSE, pad = 0)
expect_equivalent(ans, rep(0, nrow(prices)))

ans <- returns(prices, weights = weights,
               rebalance.when = FALSE, pad = NA)
expect_equivalent(ans, c(NA, rep(0, nrow(prices) - 1)))





## 2 assets, with sum(weights) != 1
weights <- c(0.5, 0.1)
ans <- returns(prices, weights = weights)
expect_equivalent(ans, drop(returns(prices) %*% weights))

weights <- c(0.5, 0.1)
ans <- returns(prices, weights = weights, rebalance.when = 2)
h <- weights/prices[2, ]
expect_equivalent(c(ans)[seq(to = length(ans), length.out = 3)],
                  c(returns(prices %*% h + (1-sum(weights)))[-1, ]))


## 2 assets, with sum(weights) != 1 || padding
weights <- c(0.5, 0.1)
ans <- returns(prices, weights = weights, rebalance.when = 1, pad = NA)
expect_true(is.na(ans[1]))
expect_true(is.finite(ans[2]))
expect_equal(length(ans), 5)

ans <- returns(prices, weights = weights, rebalance.when = 2, pad = NA)
expect_true(all(is.na(ans[1:2])))
expect_true(is.finite(ans[3]))
expect_equal(length(ans), 5)

ans <- returns(prices, weights = weights, rebalance.when = 3, pad = NA)
expect_true(all(is.na(ans[1:3])))
expect_true(is.finite(ans[4]))
expect_equal(length(ans), 5)

ans <- returns(prices, weights = weights, rebalance.when = 1, pad = 0)
expect_equal(ans[1], 0)
expect_equal(length(ans), 5)

ans <- returns(prices, weights = weights, rebalance.when = 2, pad = 0)
expect_true(all(ans[1:2] == 0))
expect_true(is.finite(ans[3]))

ans <- returns(prices, weights = weights, rebalance.when = 3, pad = 0)
expect_true(all(ans[1:3] == 0))
expect_true(is.finite(ans[4]))


### => no rebalancing at all
weights <- c(0.5, 0.1)
ans <- returns(prices, weights = weights, rebalance.when = FALSE)
expect_equivalent(ans, rep(0, nrow(prices) - 1))

weights <- c(0.0, 0.0)
ans <- returns(prices, weights = weights)
expect_equivalent(ans, rep(0, nrow(prices) - 1))

weights <- c(0.0, 0.0)
ans <- returns(prices, weights = weights, pad = 0)
expect_equivalent(ans, rep(0, nrow(prices)))




## alternative computation: new constant asset 'cash'
weights <- c(0.5, 0.1)
ans <- returns(cbind(prices,1), weights = c(weights, 1 - sum(weights)))
expect_equivalent(ans, drop(returns(prices) %*% weights))

weights <- c(0.5, 0.1)
ans <- returns(cbind(prices,1), weights = c(weights, sum(weights)),
               rebalance.when = 2)
h <- weights/prices[2, ]
expect_equivalent(ans,
                  c(returns(prices %*% h + (1-sum(weights)))[-1, ]))

