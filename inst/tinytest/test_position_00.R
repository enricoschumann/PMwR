## tests

n <- 5
amount <- seq_len(n)
timestamp <- seq_len(n)
instrument <- rep("A", n)

when <- 3
p <- position(amount, timestamp, instrument, when)
expect_equal(dim(p), c(1, 1))
expect_equivalent(unclass(p), array(6, dim = c(1, 1)))
expect_equal(attr(p, "instrument"), "A")

when <- c(2, 3)
p <- position(amount, timestamp, instrument, when)
expect_equal(dim(p), c(2, 1))
expect_equivalent(unclass(p), array(c(3, 6), dim = c(2, 1)))

when <- c(2.5, 3)
p <- position(amount, timestamp, instrument, when)
expect_equivalent(unclass(p), array(c(3, 6), dim = c(2, 1)))

when <- c(0, 3)
p <- position(amount, timestamp, instrument, when)
expect_equivalent(unclass(p), array(c(0, 6), dim = c(2, 1)))

when <- c(0, 0)
p <- position(amount, timestamp, instrument, when)
expect_equivalent(unclass(p), array(c(0, 0), dim = c(2, 1)))

when <- c(6, 6)
p <- position(amount, timestamp, instrument, when)
expect_equivalent(unclass(p), array(c(15, 15), dim = c(2, 1)))




n <- 5
amount <- seq_len(n)
timestamp <- seq_len(n)
instrument <- c("A", "B", "A", "A", "B")

when <- 3
p <- position(amount, timestamp, instrument, when)
expect_equivalent(unclass(p), array(c(4, 2), dim = c(1, 2)))

when <- c(2, 3)
p <- position(amount, timestamp, instrument, when)

when <- c(2.5, 3)
p <- position(amount, timestamp, instrument, when)

when <- c(0, 3)
p <- position(amount, timestamp, instrument, when)

when <- c(0, 0)
p <- position(amount, timestamp, instrument, when)

when <- c(6, 6)
p <- position(amount, timestamp, instrument, when)





## https://enricoschumann.net/notes/computing-positions.html
## errors
amount <- array(1:6, dim = c(2, 3))
expect_error(position(amount))


amount <- array(1:6, dim = c(2, 3))
colnames(amount) <- letters[1:3]
amount
expect_equivalent(unclass(position(amount)),
                  array(c(3, 7, 11), dim = c(1, 3)))
