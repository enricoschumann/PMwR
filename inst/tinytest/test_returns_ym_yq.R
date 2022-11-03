library("zoo", quietly = TRUE, warn.conflicts = FALSE)

t <- paste(rep(2021:2022, each = 12), 1:12)
t <- as.yearmon(t, "%Y %m")
x <- seq_along(t) + 100

expect_equivalent(unclass(returns(x)),
                  unclass(returns(zoo(x, t), period = "month")))

xxx <- returns(x, t, period = "ann")



t <- paste(rep(2021:2022, each = 4), 1:4)
t <- as.yearqtr(t, "%Y %q")
x <- seq_along(t) + 100

expect_equivalent(unclass(returns(x)),
                  unclass(returns(zoo(x, t), period = "quarter")))

xxx <- returns(x, t, period = "ann")
