library("zoo", quietly = TRUE, warn.conflicts = FALSE)

expect_equivalent(unclass(position(amount = 1:5)), as.matrix(15))

## ERROR: at least 'amount' needs to be specified
expect_error(position())


## Construct position from raw data or from journal
t <- 1:5
n <- c(1, 1, -1, 3, -4)
j <- journal(timestamp = t, amount = n)

## ... 1) check correct position for *vector* input
expect_equal(c(position(amount = n, timestamp = t, when = 4.9)),
            4)
expect_equal(c(position(amount = n, timestamp = t,
                       when = c(-10,1.4,4.9,10))),
            c(0, 1, 4, 0))

## ... 2) check correct position for *journal* input
expect_equal(c(position(j, when = 4.9)), 4)
expect_equal(c(position(j, when = c(-10,1.4,4.9,10))), c(0,1,4,0))

## ... 3) check equal output for *vector* and *journal* input
expect_equal(position(amount = n, timestamp = t, when = 4.9),
            position(j, when = 4.9))
expect_equal(position(amount = n, timestamp = t, when = c(-10,1.4,4.9,10)),
            position(j, when = c(-10,1.4,4.9,10)))



## 'when' specifications
t <- as.Date("2015-1-15") + seq(10, 350, by = 30)
expect_equal(as.numeric(position(rep(1, length(t)),
                                timestamp = t,
                                when = "endofmonth")),
            1:12)


## ... endofday
t <- as.POSIXct(c("2017-11-17 12:00:00",
                  "2017-11-17 13:00:00",
                  "2017-11-21 12:00:00"))

res <- position(c(1,1,1), timestamp = t,
                when = "endofday")

expect_equal(as.numeric(res), 2:3)
expect_equal(attr(res, "timestamp"), unique(as.Date(as.POSIXlt(t))))

res <- position(c(1,-1,1), timestamp = t,
                when = "endofday")

expect_equal(as.numeric(res), 0:1)
expect_equal(attr(res, "timestamp"), unique(as.Date(as.POSIXlt(t))))



## missing values
ans <- position(amount = 1:3,
                timestamp = c(NA, Sys.Date(), Sys.Date()),
                price = 3:1)
expect_equivalent(c(ans), 6)

ans <- position(amount = 1:3, instrument = letters[1:3],
                timestamp = c(NA, Sys.Date(), Sys.Date()),
                price = 3:1)
expect_equivalent(c(ans), 1:3)
expect_equal(instrument(ans), letters[1:3])
