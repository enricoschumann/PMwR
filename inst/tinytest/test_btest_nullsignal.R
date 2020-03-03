prices <- 1:10

## if signal returns NULL, the previous
## position is kept.
signal1 <- function()
    if (Time() == 5)
        1 else Portfolio()
signal2 <- function()
    if (Time() == 5)
        1
signal3 <- function()
    if (Time() == 5)
        1 else NULL

expect_equal(btest(prices, signal1), btest(prices, signal2))
expect_equal(btest(prices, signal1), btest(prices, signal3))
