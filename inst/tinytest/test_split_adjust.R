## no adjustments
x <- 10
t <- 0
ratio <- 5
expect_equal(split_adjust(x, t, ratio), 10)
expect_equal(split_adjust(x, t, ratio, backward = FALSE), 10)
t <- 1
expect_equal(split_adjust(x, t, ratio), 10)
expect_equal(split_adjust(x, t, ratio, backward = FALSE), 10)
t <- 2
expect_equal(split_adjust(x, t, ratio), 10)
expect_equal(split_adjust(x, t, ratio, backward = FALSE), 10)


## one adjustment
x <- c(10, 2)
t <- 2
ratio <- 5
expect_equal(split_adjust(x, t, ratio),
            c(2, 2))
expect_equal(split_adjust(x, t, ratio, backward = FALSE),
            c(10, 10))


## two adjustments
x <- c(10, 5, 5, 1)
t <- c(2, 4)
ratio <- c(2,5)
expect_equal(split_adjust(x, t, ratio),
            rep(1,4))
expect_equal(split_adjust(x, t, ratio, backward = FALSE),
            rep(10,4))

