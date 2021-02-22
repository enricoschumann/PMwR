prices <- cbind(a = 101:105, b = 201:205)

## 2 assets
weights <- c(0.8, 0.2)
ans <- returns(prices, weights = weights)
expect_equivalent(ans, c(returns(prices) %*% weights))

weights <- c(0.8, 0.2)
ans <- returns(prices, weights = weights,
               rebalance.when = 1)
expect_equivalent(ans, c(returns(prices %*% (weights/prices[1, ]))))

weights <- c(0.8, 0.2)
ans <- returns(prices, weights = weights,
               rebalance.when = 2)
tmp <- returns(prices %*% (weights/prices[2, ]))
expect_equivalent(ans, drop(tmp)[-1])

weights <- c(0.8, 0.2)
ans <- returns(prices,
               weights = weights,
               rebalance.when = c(1, 3))
tmp1 <- returns(prices[1:3, ] %*% (weights/prices[1L, ]))
tmp2 <- returns(prices[3:5, ] %*% (weights/prices[3L, ]))
expect_equivalent(ans, c(tmp1, tmp2))

weights <- c(0.8, 0.2)
ans <- returns(prices,
               weights = weights,
               rebalance.when = FALSE)
expect_equivalent(ans, rep(0, nrow(prices)-1))

weights <- rbind(c(0.8, 0.2),
                 c(0.5, 0.5))
ans <- returns(prices,
               weights = weights,
               rebalance.when = c(1, 3))
tmp1 <- returns(prices[1:3, ] %*% (weights[1L, ]/prices[1L, ]))
tmp2 <- returns(prices[3:5, ] %*% (weights[2L, ]/prices[3L, ]))
expect_equivalent(ans, c(tmp1, tmp2))


weights <- rbind(c(0.8, 0.2),
                 c(0.8, 0.2),
                 c(0.5, 0.5),
                 c(0.5, 0.5),
                 c(0.5, 0.5))
ans <- returns(prices,
               weights = weights,
               rebalance.when = c(1, 3))
tmp1 <- returns(prices[1:3, ] %*% (weights[1L, ]/prices[1L, ]))
tmp2 <- returns(prices[3:5, ] %*% (weights[3L, ]/prices[3L, ]))
expect_equivalent(ans, c(tmp1, tmp2))



## portfolio returns with weights
x <- 101:112
t <- seq_along(x)
x <- cbind(x+rnorm(length(x)), x+rnorm(length(x)))
expect_equal(returns(x[,1]),
             c(returns(x, weights = c(1,0))))
expect_equal(returns(x[,2]),
             c(returns(x, weights = c(0,1))))
## ... check attr
expect_equal(length(attributes(returns(x, weights = c(0,1)))), 2)
expect_equal(
    sort(names(attributes(returns(x, weights = c(1,0))))),
    c("contributions", "holdings"))


## ... with zoo
library("zoo", warn.conflicts = FALSE, quietly = TRUE)
expect_equal(returns(zoo(x,t))[,1],
             c(returns(zoo(x,t), weights = c(1,0))))
expect_equal(returns(zoo(x,t))[,2],
             c(returns(zoo(x,t), weights = c(0,1))))

## ... check attr with zoo
expect_equal(
    sort(names(attributes(returns(zoo(x,t), weights = c(1,0))))),
    c("class", "contributions", "holdings", "index"))

## ... match rebalance.when against timestamp
h <- attr(returns(x, weights = c(0.2, 0.8), rebalance.when = 1),
          "holdings")
expect_true(all(apply(h, 2,
                      function(x) length(unique(x))) == 1L))

h <- attr(returns(x, weights = c(0.2, 0.8), rebalance.when = 3),
          "holdings")
expect_true(all(apply(h, 2,
                      function(x) length(unique(x))) == 2L))

x <- 101:110
t <- as.Date("2017-1-1")+1:10
x <- cbind(x + rnorm(length(x)),
           x + rnorm(length(x)))
h1 <- attr(returns(x, t = t, weights = c(0.2, 0.8),
                   rebalance.when = as.Date("2017-1-4")),
           "holdings")
expect_true(all(apply(h1, 2,
                      function(x) length(unique(x))) == 2L))
h2 <- attr(returns(zoo(x, t), weights = c(0.2, 0.8),
                   rebalance.when = as.Date("2017-1-4")),
           "holdings")
expect_true(all(apply(h2, 2,
                      function(x) length(unique(x))) == 2L))
expect_equal(h1, h2)
h3 <- attr(returns(x, t = t, weights = c(0.2, 0.8),
                   rebalance.when = 3),
           "holdings")
expect_equal(h1, h3)
h4 <- attr(returns(zoo(x, t), weights = c(0.2, 0.8),
                   rebalance.when = 3),
           "holdings")
expect_equal(h1, h4)



x  <- cumprod(1+rnorm(10, sd = 0.02))
x2 <- cumprod(1+rnorm(10, sd = 0.02))
X <- cbind(x, x2)

expect_equal(
    tail(cumprod(1+returns(X, weights = c(.2,.8),
                           rebalance.when = 1)),
         1),
    c((X[nrow(X), ] / X[1, ]) %*% c(.2,.8)))

## time-weighted returns
## x <- 101:105
## expect_equal(returns(x, position = c(1, 1, 1, 1, 1)),
##             returns(x))
## expect_equal(returns(x, position = c(1, 1, 1, 1, 1), pad = NA),
##             returns(x, pad = NA))

## tmp <- returns(x)
## tmp[4] <- 0
## expect_equal(returns(x, position = c(1, 1, 1, 0, 0)),
##             tmp)

## expect_equal(returns(x, position = c(1,1,2,2,3)),
##             returns(x))
## expect_equal(returns(x, position = c(0,0,0,0,0)),
##             rep(0, 4))

## pos <- c(1,1,1,2,2,0)
## price <- c(100,100,100,100,100,100)
## dim(pos) <- dim(price) <- c(3, 2)
## expect_equal(returns(price, position = pos), returns(price[ ,1]))
## expect_equal(returns(price, position = pos),
##             rowSums((price*pos / rowSums(price*pos))[-3, ] * returns(price)))

## pos[ ,2] <- 0
## expect_equal(returns(price, position = pos),
##             returns(price[,1]))

## pos1 <- c(1,1,1,2,2,2)
## pos2 <- pos1 * 2
## price <- c(101,102,103,103,105,107)
## dim(price) <- dim(pos2) <- dim(pos1) <- c(3,2)

## expect_equal(returns(price, position = pos1),
##             rowSums((price*pos1 / rowSums(price*pos1))[-3, ] * returns(price)))
## expect_equal(returns(price, position = pos1),
##             returns(price, position = pos2))


## position: 1 asset
prices <- cbind(a = 101:105, b = 201:205)
when <- 1

pos <- c(1, 0)
target <- returns(prices[, 1])
expect_equivalent(returns(prices, position = pos, rebalance.when = when),
                  target)

pos <- c(0, 1)
target <- returns(prices[, 2])
expect_equivalent(returns(prices, position = pos, rebalance.when = when),
                  target)

pos <- c(0, 0)
target <- numeric(nrow(prices) - 1)
expect_equivalent(returns(prices, position = pos, rebalance.when = when),
                  target)

##
when <- 2

pos <- c(1, 0)
target <- returns(prices[, 1])[-1]
expect_equivalent(returns(prices, position = pos, rebalance.when = when),
                  target)

pos <- c(0, 1)
target <- returns(prices[, 2])[-1]
expect_equivalent(returns(prices, position = pos, rebalance.when = when),
                  target)

pos <- c(0, 0)
target <- numeric(nrow(prices) - 1)[-1]
expect_equivalent(returns(prices, position = pos, rebalance.when = when),
                  target)



## position: 2 assets
prices <- cbind(a = 101:105, b = 201:205)
pos <- rbind(c(1,0),
             c(1,2))
when <- c(1, 3)

target <- c(returns(prices[,1])[1:2],
            returns(prices %*% c(1,2))[3:4])
expect_equivalent(returns(prices,
                          position = pos,
                          rebalance.when = when),
                  target)

## ------ zero positions ----------------------------------------
##... ==> no "rebalance.when"

prices <- cbind(a = 101:105, b = 201:205)
pos <- rbind(c(0,0))

target <- rep(0, nrow(prices) - 1)
expect_equivalent(returns(prices,
                          position = pos),
                  target)
expect_equivalent(returns(prices,
                          weights = pos),
                  target)

target <- c(NA, rep(0, nrow(prices) - 1))
expect_equivalent(returns(prices,
                          position = pos,
                          pad = NA),
                  target)
expect_equivalent(returns(prices,
                          weights = pos,
                          pad = NA),
                  target)

##... ==> with "rebalance.when"

prices <- cbind(a = 101:105, b = 201:205)
pos <- rbind(c(0,0))

target <- rep(0, nrow(prices) - 1)
expect_equivalent(returns(prices,
                          position = pos,
                          rebalance.when = c(1, 3)),
                  target)
expect_equivalent(returns(prices,
                          weights = pos,
                          rebalance.when = c(1, 3)),
                  target)

target <- c(NA, rep(0, nrow(prices) - 1))
expect_equivalent(returns(prices,
                          position = pos,
                          rebalance.when = c(1, 3),
                          pad = NA),
                  target)
expect_equivalent(returns(prices,
                          weights = pos,
                          rebalance.when = c(1, 3),
                          pad = NA),
                  target)



## ------ zero positions ----------------------------------------

prices <- cbind(a = 101:105, b = 201:205)
pos <- rbind(c(0.4, 0.2),
             c(  0,   0))

target <- c(sum((prices[2, ]/prices[1, ]-1)*pos[1, ]),
            rep(0, nrow(prices) - 2))
expect_equivalent(returns(prices,
                          weights = pos,
                          rebalance.when = c(1, 2)),
                  target)

target <- c(returns(colSums(t(prices) * pos[1, ]))[1],
            rep(0, nrow(prices) - 2))
expect_equivalent(returns(prices,
                          position = pos,
                          rebalance.when = c(1, 2)),
                  target)

target <- c(sum((prices[3, ]/prices[2, ]-1)*pos[1, ]),
            rep(0, nrow(prices) - 3))
expect_equivalent(returns(prices,
                          weights = pos,
                          rebalance.when = c(2, 3)),
                  target)

target <- c(returns(colSums(t(prices) * pos[1, ]))[2],
            rep(0, nrow(prices) - 3))
expect_equivalent(returns(prices,
                          position = pos,
                          rebalance.when = c(2, 3)),
                  target)


## ------ matching of timestamps --------------------------------

prices <- cbind(a = 101:105, b = 201:205)
pos <- rbind(c(0.4, 0.2),
             c(0.3, 0.7))

t <- as.Date("2015-1-1") + 1:nrow(prices)
expect_equivalent(
    returns(prices,
            weights = pos,
            rebalance.when = c(1, 3)),
    returns(prices,
            t = t,
            weights = pos,
            rebalance.when = t[c(1, 3)]))

expect_equivalent(
    returns(prices,
            weights = pos,
            rebalance.when = c(1, 3)),
    returns(prices,
            t = 2:6, ## <== integer
            weights = pos,
            rebalance.when = c(2L, 4L))) ## <== integer

expect_equivalent(
    returns(prices,
            weights = pos,
            rebalance.when = c(1, 3)),
    returns(prices,
            t = as.numeric(2:6), ## <== numeric
            weights = pos,
            rebalance.when = c(2L, 4L))) ## <== integer

expect_equivalent(
    returns(prices,
            weights = pos,
            rebalance.when = c(1, 3)),
    returns(prices,
            t = 2:6, ## <== integer
            weights = pos,
            rebalance.when = c(2, 4))) ## <== numeric


## ------ weights for each row ----------------------------------

prices <- cbind(a = 101:105, b = 201:205)
w <- sample(1:9/10, size = length(prices), replace = TRUE)
dim(w) <- dim(prices)

expect_equivalent(
    rowSums(returns(prices) * w[-nrow(w), ]),
    returns(x = prices, weights = w))

expect_equivalent(
    rowSums(returns(prices) * w[-nrow(w), ]),
    returns(x = prices, weights = w[-nrow(w), ]))



## ------ NAs ----------------------------------

prices <- cbind(a = 101:105, b = 201:205)
w <- c(0.6, 0.4)
R0 <- returns(x = prices, weights = w, rebalance.when = 1)

### NA at a non-rebalancing timestamp
prices[3, 1] <- NA
R1 <- returns(x = prices, weights = w, rebalance.when = 1)
expect_equivalent(R0[is.finite(R1)], R1[is.finite(R1)])
