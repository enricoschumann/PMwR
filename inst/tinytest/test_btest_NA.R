prices <- 1:10
signal <- function()
    if (Time() < 5)
        1 else 0
expect_equal(bt1 <- btest(prices, signal)$wealth,
             c(0, 0, 1, 2, 3, 4, 4, 4, 4, 4))

## signal returns position
prices[7:10] <- NA
signal <- function()
    if (Time() < 5)
        1 else 0
expect_equal(bt2 <- btest(prices, signal)$wealth,
             c(0, 0, 1, 2, 3, 4, 4, 4, 4, 4))
expect_equal(bt1, bt2)

prices1  <- prices2 <- 1:10
prices2[7:10] <- NA
prices <- cbind(prices1, prices2)
signal <- function()
    if (Time() < 5)
        c(1,1) else c(1,0)
expect_equal(bt3 <- btest(list(prices), signal)$wealth,
             c(0, 0, 2, 4, 6, 8, 9, 10, 11, 12))


## signal returns weight
prices <- 1:5
prices[4:5] <- NA
signal <- function() {
    if (Time(0) <= 3)
        0.5 else 0
}
btest(prices, signal, initial.cash = 100, convert.weights = TRUE)$wealth

prices1  <- prices2 <- 1:10
prices2[7:10] <- NA
prices <- cbind(prices1, prices2)
signal <- function()
    if (Time() < 5)
        c(1,1) else c(1,0)
expect_equal(bt3 <- btest(list(prices), signal)$wealth,
             c(0, 0, 2, 4, 6, 8, 9, 10, 11, 12))

