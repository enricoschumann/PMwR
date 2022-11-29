prices <- cbind(1:10, 101:110)
timestamp <- Sys.Date() + nrow(prices)

signal <- function() {
    c(Time()+0.1, Time()+0.2)
}

do.rebalance <- function() {
if (Time(0) %% 2 == 0)
        c(TRUE, FALSE)
    else
        c(FALSE, TRUE)
}

bt <- btest(list(prices), signal = signal,
            do.rebalance = do.rebalance)

expect_equal((journal(bt)["asset 1"])$timestamp,
             seq(2, 10, by = 2))

expect_equal((journal(bt)["asset 2"])$timestamp,
             seq(3, 10, by = 2))
