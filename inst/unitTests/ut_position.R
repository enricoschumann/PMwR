
require("PMwR")

## construct from raw data
t <- 1:5
n <- c(1, 1, -1, 3, -4)
position(amount = n, timestamp = t, when = 4.9)
position(amount = n, timestamp = t, when = c(-10,1.4,4.9))

## from journal
j <- journal(timestamp = t, amount = n)
position(j, when = 4.9)
position(j, when = c(-10,1.4,4.9))


