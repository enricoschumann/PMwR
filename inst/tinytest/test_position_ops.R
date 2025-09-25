## operations: +, -, /, ...
require(PMwR);require(tinytest)
p1 <- position(c(a = 1, b = 2))
p2 <- position(c(b = 2, c = 3))

expect_equal(p1 + p2,
             position(c(a = 1, b = 4, c = 3)))

expect_equal(p1 - p2,
             position(c(a = 1, b = 0, c = -3)))

expect_equal(-p1,
             position(c(a = -1, b =-2)))

expect_equal(+p1, p1)

expect_equal(p1 > p2,
             c(a = TRUE,  b = FALSE, c = FALSE))
expect_equal(p1 < p2,
             c(a = FALSE, b = FALSE, c = TRUE))
expect_equal(p1 >= p2,
             c(a = TRUE,  b = TRUE, c = FALSE))
expect_equal(p1 <= p2,
             c(a = FALSE, b = TRUE, c = TRUE))
expect_equal(p1 == p2,
             c(a = FALSE, b = TRUE, c = FALSE))
expect_equal(p1 | p2,
             c(a = TRUE,  b = TRUE, c = TRUE))
expect_equal(!(p1 | p2),
             !c(a = TRUE,  b = TRUE, c = TRUE))
expect_equal(!p1,
             c(a = TRUE,  b = TRUE, c = TRUE))


p1 <- position(c(a = 1, b = 2))
p2 <- position(c(b = NA, c = NA))

expect_equal(p1 + p2,
             position(c(a = 1, b = NA, c = NA)))




## unary !
p1 <- position(c(a = 1, b = 0, c = 1))
expect_equivalent(drop(!!p1),
                  c(TRUE, FALSE, TRUE))



##
x <- position(amount = 1, instrument = c("a"))
y <- position(amount = 1:2, instrument = c("a","b"))
expect_equal(x, +x)
expect_equal(y, +y)
xy <- x+y
expect_true(inherits(xy, "position"))
expect_true(is.na(attr(xy, "timestamp")))
expect_equal(dim(xy), c(1, 2))
expect_equal(attr(xy, "instrument"), c("a", "b"))
expect_equal(dimnames(xy), list("", c("a", "b")))
