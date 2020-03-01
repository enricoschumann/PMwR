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
ratio <- c(2, 5)
expect_equal(split_adjust(x, t, ratio),
            rep(1, 4))
expect_equal(split_adjust(x, t, ratio, backward = FALSE),
            rep(10, 4))


## adjust shares outstanding (=> invert ratio)
x <- c(100, 100, 200, 200, 400, 1200)
t <- c(3, 5, 6)
ratio <- c(2, 2, 3)
expect_equal(split_adjust(x, t, 1/ratio, backward = FALSE),
             rep(100, 6))

x <- c(100, 100, 200, 200, 400, 1200)
t <- c(3, 5, 6)
ratio <- c(2, 2, 3)
expect_equal(split_adjust(x, t, 1/ratio, backward = TRUE),
             rep(1200, 6))

### .... the same, but somewhat less intuitive
x     <- c(100, 100, 200, 200, 400, 1200)
ratio <- c(  1,   1,   2,   1,   2,    3)
### ----------------------- ^  shares remain the same
expect_equal(split_adjust(x, t = 1:6, 1/ratio, backward = TRUE),
             rep(1200, 6))


## several adjustments on same day
x     <- c(100, 400, 800)
ratio <- c(2, 2, 2)
t     <- c(2, 2, 3)

expect_equal(split_adjust(x, t = t, 1/ratio),
             rep(800, 3))

expect_equal(split_adjust(x, t = t, 1/ratio, backward = FALSE),
             rep(100, 3))

