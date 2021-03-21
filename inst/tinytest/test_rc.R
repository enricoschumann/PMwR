## return contribution

prices <- 1:10
prices <- cbind(A = prices, B = prices + 0.5)
signal <- function()
    if (Time() < 5L)
        c(1, 1) else c(0, 0)
bt <- btest(list(prices), signal,
            initial.cash = 100,
            instrument = colnames(prices),
            include.data = TRUE)

weights <- cbind(prices, 1) *
    position(bt, include.cash = TRUE) / bt$wealth
C <- rbind(0, returns(cbind(prices, 1)) * weights[-nrow(prices), ])
expect_equal(rowSums(C), returns(bt$wealth, pad = 0))


## -----------------------------
w <- c(0.5,0.5)
R <- c(0.1, 0.2)
ans <- rc(t(R), t(w))
expect_equal(ans$period_contributions$segment_1, (w*R)[1])
expect_equal(ans$period_contributions$segment_2, (w*R)[2])

ans <- rc(t(R), t(w), segments = c("A", "B"))
expect_equal(ans$period_contributions$A, (w*R)[1])
expect_equal(ans$period_contributions$B, (w*R)[2])
