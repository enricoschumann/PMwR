expect_equal(pl(amount = c(1,-1),
               price  = c(1,2))[[1]][["volume"]], 2)

expect_equal(pl(amount = c(1,1,1,1,-4),
               price  = c(1,1,1,1, 1))[[1]][["volume"]], 8)


## PL at completely different times
j <- journal(amount = c(1, -1),
             price = 1:2,
             timestamp = 1:2)

res <- pl(j, along.timestamp = 6:10, vprice = 6:10)
expect_equivalent(res[[1]][["volume"]], rep(2, 5))
## TODO res <- pl(j, along.timestamp = -5:-1, vprice = 1:5)

j <- journal(amount = c(1, -1, 1, -1),
             price = c(1, 2, 1, 2),
             timestamp = c(0.1,0.2,0.3,0.4))

res <- pl(j, along.timestamp = 1:5, vprice = 1:5)
expect_equivalent(res[[1]][["volume"]], rep(4, 5))


## PL at completely different times: Date
j <- journal(amount = c(1, -1),
             price = 1:2,
             timestamp = as.Date("2018-1-1") + 0:1)

res <- pl(j, along.timestamp = as.Date("2018-1-1") + 6:10, vprice = 6:10)
expect_equivalent(res[[1]][["volume"]], rep(2, 5))
## TODO res <- pl(j, along.timestamp = -5:-1, vprice = 1:5)

j <- journal(amount = c(1, -1, 1, -1),
             price = c(1, 2, 1, 2),
             timestamp = as.Date("2018-1-1")-4:1)

res <- pl(j, along.timestamp = as.Date("2018-1-1") + 1:5,
          vprice = 1:5)
expect_equivalent(res[[1]][["volume"]], rep(4, 5))

