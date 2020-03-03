prices <- 1:10
signal <- function()
    Time()
tc <- function()
    Time()  ## will be 0:9

journal(bt <- btest(prices, signal, tc = tc))
expect_equal(bt$cum.tc,
             c(0, cumsum(prices[-1]*seq_len(9))))
             ###                    ^^^^^^^^^^ tc

journal(bt <- btest(prices, signal, tc = 1))

expect_equal(bt$cum.tc,
             c(0, cumsum(prices[-1])))
