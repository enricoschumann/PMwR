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
expect_equivalent(ans,
                  c(0, returns(prices %*% h + (1-sum(weights)))[-1, ]))

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
ans <- returns(cbind(prices,1), weights = c(weights, sum(weights)))
expect_equivalent(ans, drop(returns(prices) %*% weights))

weights <- c(0.5, 0.1)
ans <- returns(cbind(prices,1), weights = c(weights, sum(weights)),
               rebalance.when = 2)
h <- weights/prices[2, ]
expect_equivalent(ans,
                  c(0, returns(prices %*% h + (1-sum(weights)))[-1, ]))

