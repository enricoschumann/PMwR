## no adjustments
x <- 10
div <- 5
t <- 0
expect_equal(div_adjust(x, t, div), 10)
expect_equal(div_adjust(x, t, div, backward = FALSE), 10)
t <- 1
expect_equal(div_adjust(x, t, div), 10)
expect_equal(div_adjust(x, t, div, backward = FALSE), 10)
t <- 2
expect_equal(div_adjust(x, t, div), 10)
expect_equal(div_adjust(x, t, div, backward = FALSE), 10)


## one adjustment
x <- c(10, 5)
div <- 5
t <- 2
expect_equal(div_adjust(x, t, div),
            c(5, 5))
expect_equal(div_adjust(x, t, div, backward = FALSE),
            c(10, 10))


## two adjustments
x <- c(10,9,9,8)
div <- 1
t <- c(2,4)
expect_equal(div_adjust(x, t, div), rep(8, 4))
expect_equal(div_adjust(x, t, div, backward = FALSE), rep(10, 4))


## ADDITIVE tests
x <- c(10,10,8,8,8)
div <- 2
t <- 3
expect_equal(
    div_adjust(x, t, div, additive = TRUE),
    rep(8, 5))
expect_equal(
    div_adjust(x, t, div, additive = TRUE, backward = FALSE),
    rep(10, 5))


x <- c(10,10,8,8,6)
div <- c(2, 2)
t <- c(3, 5)
expect_equal(
    div_adjust(x, t, div, additive = TRUE),
    rep(6, 5))
expect_equal(
    div_adjust(x, t, div, additive = TRUE, backward = FALSE),
    rep(10, 5))



## MULTIPLICATIVE tests
### 1 div
x <- c(10,11,10,11,12)
div <- 2
t <- 3
R <- returns(x, pad = 0)
R[t] <- (x[t]+div)/x[t-1] - 1

expect_equivalent(
    div_adjust(x, t, div),
    scale1(cumprod(R+1), level = x[length(x)], when = length(x)))
expect_equivalent(
    div_adjust(x, t, div, backward = FALSE),
    scale1(cumprod(R+1), level = x[1], when = 1))

### 2 divs
x <- c(10,11,9,10,8)
div <- c(2, 2)
t <- c(3, 5)
R <- returns(x, pad = 0)
R[t] <- (x[t]+div)/x[t-1] - 1

expect_equivalent(
    div_adjust(x, t, div),
    scale1(cumprod(R+1), level = x[length(x)], when = length(x)))
expect_equivalent(
    div_adjust(x, t, div, backward = FALSE),
    scale1(cumprod(R+1), level = x[1], when = 1))
