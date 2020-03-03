prices <- 1:10
prices <- cbind(A = prices, B = prices + 0.5)
signal <- function()
    if (Time() < 5L)
        c(1,1) else c(0,0)
bt <- btest(list(prices), signal,
            initial.cash = 100,
            instrument = colnames(prices),
            include.data = TRUE)
weights <- cbind(prices, 1) *
    position(bt, include.cash = TRUE) / bt$wealth
C <- rbind(0, returns(cbind(prices, 1)) * weights[-nrow(prices), ])
expect_equal(rowSums(C), returns(bt$wealth, pad = 0))
