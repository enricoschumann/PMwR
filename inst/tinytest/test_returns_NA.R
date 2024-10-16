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


## YEAR
x <- 100 + 0:440/100
t <- as.Date("2019-1-1") + seq_along(x) -1
returns(x, t = t, period = 2019)
returns(x, t = t, period = 2020)
returns(x, t = t, period = 2021)




## MONTH
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


x <- 100:500
x[-c(1, length(x))] <- NA
t <- as.Date("2019-1-1") + seq_along(x) -1
i <- c(1:5, seq(to = length(x), length.out = 5))
x <- x[i]
t <- t[i]
r <- as.matrix(returns(x, t = t, period = "Month"))
expect_true(is.na(r[1, 1]))
expect_true(is.na(r[1, 13]))



x <- 100:190
x[31] <- NA
t <- as.Date("2019-12-1") + seq_along(x) - 1
returns(x, t = t, period = "Month")




## 'TOTAL'
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

x <- 100:190
x[] <- NA  ## all NAs
t <- as.Date("2019-12-1") + seq_along(x) - 1
ans <- returns(x, t = t, period = "total", na.rm = TRUE)
expect_equivalent(c(ans), x[length(x)]/x[2L]-1)





## period 'YTD'
x <- 100:150
t <- as.Date("2019-12-1") + seq_along(x) - 1
ans <- suppressWarnings(returns(x, t = t, period = "ytd"))
expect_equivalent(c(ans), x[51]/x[31L]-1)

x[length(x)] <- NA
ans <- suppressWarnings(returns(x, t = t, period = "ytd"))
expect_true(is.na(ans))

ans <- suppressWarnings(returns(x, t = t, period = "ytd", na.rm = TRUE))
expect_equivalent(c(ans), x[50]/x[31L]-1)

ans <- returns(x, t = t, period = "ytd!", na.rm = TRUE)
expect_equivalent(c(ans), x[50]/x[31L]-1)  ## ! : no warning





## MTD
x <- 100:150
t <- as.Date("2019-12-1") + seq_along(x) - 1
ans <- suppressWarnings(returns(x, t = t, period = "mtd"))
expect_equivalent(x[length(x)]/x[match("2019-12-31", t)] - 1,
                  c(ans))

x <- 100:150
t <- as.Date("2019-12-1") + seq_along(x) - 1
ans <- suppressWarnings(returns(x, t = t, period = "ytd"))
expect_equivalent(x[length(x)]/x[match("2019-12-31", t)] - 1,
                  c(ans))

x <- 100:150
t <- as.Date("2019-12-1") + seq_along(x) - 1
x[length(x)] <- NA
ans <- suppressWarnings(returns(x, t = t, period = "mtd"))
expect_true(is.na(c(ans)))

ans <- suppressWarnings(returns(x, t = t, period = "mtd", na.rm = TRUE))
expect_equivalent(x[length(x)-1]/x[match("2019-12-31", t)] - 1,
                  c(ans))

x[match("2019-12-31", t)] <- NA ## should use 2019-12-30
ans <- suppressWarnings(returns(x, t = t, period = "mtd", na.rm = TRUE))
expect_equivalent(x[length(x) - 1]/x[match("2019-12-30", t)] - 1,
                  c(ans))
expect_equal(c(attr(ans, "t")),
             as.Date(c("2019-12-30", "2020-01-19")))


## MTD
x <- 100:150
t <- as.Date("2019-12-1") + seq_along(x) - 1
x <- cbind(x, x)
x[nrow(x), 1] <- NA
x[nrow(x)+c(-1:0), 2] <- NA
ans <- suppressWarnings(returns(x, t = t, period = "mtd"))
expect_true(all(is.na(ans)))

ans <- suppressWarnings(returns(x, t = t, period = "mtd", na.rm = TRUE))
expect_equivalent(c(149, 148)/x[match("2019-12-31", t), ] - 1,
                  c(ans))


t <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "1 day")
x <- seq_along(t)/10 + 100
z <- zoo(x, t)
## z <- cbind(z,z,z)
returns(z, period = "mtd")




## YTM
x <- 100:150
t <- as.Date("2019-12-1") + seq_along(x) - 1
ans <- returns(x, t = t, period = "ytm")





## HOUR
t <- seq(as.POSIXct("2020-12-30"),
         by = "5 min",
         length.out = 70)
x <- seq_along(t)/10 + 100
z <- zoo(x, t)
returns(z, period = "hour")

z[2] <- NA
returns(z, period = "hour")


## DAY
t <- seq(as.POSIXct("2020-12-30"),
         by = "5 min",
         length.out = 70)
x <- seq_along(t)/10 + 100
z <- zoo(x, t)

returns(z, period = "day")
expect_error(returns(z, period = "day", complete.first = FALSE))

x <- 100:150
t <- as.Date("2019-12-1") + seq_along(x) - 1
expect_equivalent(returns(x),
                  c(returns(x = x, t = t, period = "day")))

x <- 100:150
x[5] <- NA
t <- as.Date("2019-12-1") + seq_along(x) - 1
expect_equivalent(returns(x),
                  c(returns(x = x, t = t, period = "day")))



## ANN
Lmax <- if (Sys.getenv("ES_PACKAGE_TESTING_462643383279") == "true")
            1e4 else 750

for (L in 366:Lmax) {
    x <- rnorm(L, sd = 0.001) + 1
    t <- as.Date("2017-12-1") + seq_along(x) - 1
    per <- as.numeric(t[length(t)] - t[1], units = "days")/365

    passed <- expect_equivalent((x[length(x)] / x[1])^(1/per)-1,
                            c(returns(x = x, t = t, per = "ann")))
}


for (L in 1:Lmax) {
    x <- rnorm(L, sd = 0.001) + 1
    t <- as.Date("2019-12-1") + seq_along(x) - 1
    per <- as.numeric(t[length(t)] - t[1], units = "days")/365


    passed <- expect_equivalent((x[length(x)] / x[1])^(1/per)-1,
                      c(returns(x = x, t = t, per = "ann!")))
}


for (L in 1:Lmax) {
    x <- rnorm(L, sd = 0.001) + 1
    x[c(20, 70)] <- NA
    t <- as.Date("2019-12-1") + seq_along(x) - 1
    per <- as.numeric(t[length(t)] - t[1], units = "days")/365


    passed <- expect_equivalent((x[length(x)] / x[1])^(1/per)-1,
                      c(returns(x = x, t = t, per = "ann!")))
}



## with NAs
x <- rnorm(400, sd = 0.001) + 1
t <- as.Date("2017-12-1") + seq_along(x) - 1
per <- as.numeric(t[length(t)] - t[1], units = "days")/365
x[1] <- NA
expect_true(is.na(res <- returns(x = x, t = t, per = "ann")))
expect_true(all(attr(res, "t") == range(t)))

res <- returns(x = x, t = t, per = "ann", na.rm = TRUE)
expect_true(all(attr(res, "t") == range(t[-1])))
per <- as.numeric(t[length(t)] - t[2], units = "days")/365
expect_equivalent((x[length(x)] / x[2])^(1/per)-1,
                  c(returns(x = x, t = t, per = "ann!", na.rm = TRUE)))
