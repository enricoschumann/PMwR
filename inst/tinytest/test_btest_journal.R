prices <- 1:10
signal <- function()
    if (Time() < 5)
        1 else 0

j <- journal(btest(prices, signal))
expect_equal(j,
             journal(amount = c(1, -1),
                     timestamp = c(2, 6),
                     instrument = "asset 1",
                     price = c(2, 6)))

j <- journal(btest(prices, signal, instrument = "A"))
expect_equal(j,
             journal(amount = c(1, -1),
                     timestamp = c(2, 6),
                     instrument = "A",
                     price = c(2, 6)))
expect_equal(j,
             journal(amount = c(1, -1),
                     timestamp = c(2, 6),
                     instrument = c("A", "A"),
                     price = c(2, 6)))

prices <- 1:10
prices <- cbind(A = prices, B = prices+0.5)
signal <- function()
    if (Time() < 5L)
        c(1,1) else c(0,0)
j <- journal(btest(list(prices), signal))
expect_equal(j,
             journal(amount = c(1, 1, -1, -1),
                     timestamp = c(2, 2, 6, 6),
                     instrument = c("A", "B", "A", "B"),
                     price = c(2, 2.5, 6, 6.5)))

j <- journal(btest(list(prices), signal,
                   ## overwrite instruments
                   instrument = c("a", "b")))
expect_equal(j,
             journal(amount = c(1, 1, -1, -1),
                     timestamp = c(2, 2, 6, 6),
                     instrument = c("a", "b", "a", "b"),
                     price = c(2, 2.5, 6, 6.5)))

signal <- function()
    if (Time() < 5L)
        c(0,1) else c(0,0)
j <- journal(btest(list(prices), signal))
expect_equal(j,
             journal(amount = c(1, -1),
                     timestamp = c(2, 6),
                     instrument = "B",
                     price = c(2.5, 6.5)))
