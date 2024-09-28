
library("zoo", quietly = TRUE, warn.conflicts = FALSE)
library("PMwR")
library("NMOF")
library("tinytest")

## numeric vector
x <- 101:112
x[3] <- NA

expect_equivalent(returns(x),
                  x[-1]/x[-length(x)] - 1)

expect_equivalent(returns(x, pad = NA),
                  c(NA, x[-1]/x[-length(x)] - 1))


x <- 101:112
x[] <- NA
expect_equivalent(returns(x),
                  x[-1]/x[-length(x)] - 1)
expect_equivalent(returns(x, pad = NA),
                  c(NA, x[-1]/x[-length(x)] - 1))

x <- 101:112
x <- cbind(x, x , x)
x[] <- NA
expect_equivalent(returns(x),
                  x[-1, ]/x[-nrow(x), ] - 1)
expect_equivalent(returns(x, pad = NA),
                  rbind(NA, x[-1, ]/x[-nrow(x), ] - 1))




##
x <- 100:140
t <- as.Date("2019-1-1") + seq_along(x) -1
## returns(x, t = t, period = "Month")
## ##       Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec  YTD
## ## 2019 30.0 7.7                                         40.0

r <- as.matrix(returns(x, t = t, period = "Month"))
expect_equivalent(r[1, 1], x[31]/x[1] - 1)
expect_equivalent(r[1, 13], x[length(x)]/x[1] - 1)




x <- 100:140
x[5] <- NA
t <- as.Date("2019-1-1") + seq_along(x) -1
## returns(x, t = t, period = "Month")
## ##       Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec  YTD
## ## 2019 30.0 7.7                                         40.0

r <- as.matrix(returns(x, t = t, period = "Month"))
expect_equivalent(r[1, 1], x[31]/x[1] - 1)
expect_equivalent(r[1, 13], x[length(x)]/x[1] - 1)




x <- 100:140
x[31] <- NA
t <- as.Date("2019-1-1") + seq_along(x) -1
## returns(x, t = t, period = "Month")
## ##      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec YTD
## ## 2019  NA  NA                                          NA
r <- as.matrix(returns(x, t = t, period = "Month"))
expect_true(is.na(r[1, 1]))
expect_true(is.na(r[1, 13]))




x <- 100:190
x[31] <- NA
t <- as.Date("2019-12-1") + seq_along(x) - 1
returns(x, t = t, period = "Month")




## period 'total'
x <- 100:190
x[31] <- NA
t <- as.Date("2019-12-1") + seq_along(x) - 1
ans <- returns(x, t = t, period = "total")
expect_equivalent(c(ans), x[length(x)]/x[1L]-1)

x <- 100:190
x[1] <- NA
t <- as.Date("2019-12-1") + seq_along(x) - 1
ans <- returns(x, t = t, period = "total")
expect_true(is.na(ans))

x <- 100:190
x[1] <- NA
t <- as.Date("2019-12-1") + seq_along(x) - 1
ans <- returns(x, t = t, period = "total", na.rm = TRUE)
expect_equivalent(c(ans), x[length(x)]/x[2L]-1)



## period 'ytd'
x <- 100:150
t <- as.Date("2019-12-1") + seq_along(x) - 1
ans <- returns(x, t = t, period = "ytd")
expect_equivalent(c(ans), x[51]/x[31L]-1)

x[length(x)] <- NA
ans <- returns(x, t = t, period = "ytd")
expect_true(is.na(ans))

ans <- returns(x, t = t, period = "ytd", na.rm = TRUE)
expect_equivalent(c(ans), x[50]/x[31L]-1)
