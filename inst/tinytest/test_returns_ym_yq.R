library("PMwR")
library("tinytest")
library("zoo", quietly = TRUE, warn.conflicts = FALSE)

t <- paste(rep(2021:2022, each = 12), 1:12)
t <- as.yearmon(t, "%Y %m")
x <- seq_along(t) + 100

expect_equivalent(unclass(returns(x)),
                  unclass(returns(zoo(x, t), period = "month")))

expect_equivalent(unclass(returns(x)),
                  unclass(returns(x, t=t, period = "month")))

xxx <- returns(x, t, period = "ann")

returns(cbind(a=zoo(x, t),
              b=zoo(x, t)),
        period = "ann")


t <- paste(rep(2021:2022, each = 4), 1:4)
t <- as.yearqtr(t, "%Y %q")
x <- seq_along(t) + 100

expect_equivalent(unclass(returns(x)),
                  unclass(returns(zoo(x, t), period = "quarter")))

xxx <- returns(x, t, period = "ann")
xxx <- returns(cbind(a=zoo(x, t),
                   b=zoo(x, t)),
             period = "ann")

expect_equivalent(unclass(returns(zoo(x, t), period = "ann")),
                  (x[length(x)]/x[1L])^(4/(length(x)-1)) - 1)

xxx <- returns(x[1:2], t[1:2], period = "ann")
expect_equivalent(unclass(xxx), x[2]/x[1]-1)

xxx <- returns(x[1:2], t[1:2], period = "ann!")
expect_equivalent(unclass(xxx), (x[2]/x[1])^4-1)



xxx <- returns(x, t, period = "ytd")
