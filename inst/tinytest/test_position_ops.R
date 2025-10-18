## operations: +, -, /, ...
require(PMwR);require(tinytest)
p1 <- position(c(a = 1, b = 2))
p2 <- position(c(b = 2, c = 3))

## +
e <- position(c(a = 1, b = 4, c = 3))
expect_equivalent(unclass(p1 + p2), unclass(e))
expect_equal(attr(p1 + p2, "instrument"),
             attr(e, "instrument"))
expect_equal(attr(p1 + p2, "timestamp"),
             attr(e, "timestamp"))
expect_true(inherits(p1 + p2, "position"))

e <- position(c(a = 1, b = 0, c = -3))
expect_equivalent(unclass(p1 - p2), unclass(e))
expect_equal(attr(p1 - p2, "instrument"),
             attr(e, "instrument"))
expect_equal(attr(p1 - p2, "timestamp"),
             attr(e, "timestamp"))
expect_true(inherits(p1 - p2, "position"))


expect_equal(-p1,
             position(c(a = -1, b =-2)))

expect_equal(+p1, p1)
expect_equal(-p1, -1*p1)

expect_equal(drop(p1 > p2),
             c(a = TRUE,  b = FALSE, c = FALSE))
expect_equal(drop(p1 < p2),
             c(a = FALSE, b = FALSE, c = TRUE))
expect_equal(drop(p1 >= p2),
             c(a = TRUE,  b = TRUE, c = FALSE))
expect_equal(drop(p1 <= p2),
             c(a = FALSE, b = TRUE, c = TRUE))
expect_equal(drop(p1 == p2),
             c(a = FALSE, b = TRUE, c = FALSE))

expect_equal(drop(p1 | p2),
             c(a = TRUE,  b = TRUE, c = TRUE))
expect_equal(drop(!(p1 | p2)),
             !c(a = TRUE,  b = TRUE, c = TRUE))
expect_equal(drop(!p1),
             c(a = FALSE,  b = FALSE))


p1 <- position(c(a = 1, b = 2))
p2 <- position(c(b = NA, c = NA))
e <- position(c(a = 1, b = NA, c = NA))
expect_equivalent(unclass(p1 + p2), unclass(e))
expect_equal(attr(p1 + p2, "instrument"),
             attr(e, "instrument"))
expect_equal(attr(p1 + p2, "timestamp"),
             attr(e, "timestamp"))
expect_true(inherits(p1 + p2, "position"))





p1 <- position(journal(instrument = c("a", "a", "b", "a"),
                      amount     = c(1, 1, 1, 1),
                      timestamp  = c(1, 2, 3, 4)),
               when = 1:4)

p2 <- position(journal(instrument = c("c", "c"),
                       amount     = c(1, 2),
                       timestamp  = c(1, 4)),
               when = 1:4)
dput(p1 + p2)

p1 - p2

p1 / p2
p2 / p1


expect_error(p1*p2)

## ... 'when' differs from p2
p3 <- position(journal(instrument = c("c", "c"),
                       amount     = c(1, 2),
                       timestamp  = c(1, 4)),
               when = 4:1)

expect_equal(p1 + p2, p1 + p3)





## unary !
p1 <- position(c(a = 1, b = 0, c = 1))
expect_equivalent(drop(!!p1),
                  c(TRUE, FALSE, TRUE))

p <- drop(!position(c(a = 1, b = 0, c = 1)))
expect_equivalent(p,
                  !c(TRUE, FALSE, TRUE))


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



p <- position(amount = 1:2, instrument = c("a","b"))
expect_equivalent(as.matrix(p + 1),   t(c(2, 3)))
expect_equivalent(as.matrix(p + 1:2), t(c(2, 4)))


p <- position(journal(instrument = c("a", "a", "a", "a"),
                      amount     = c(1, 1, 1, 1),
                      timestamp  = c(1, 2, 3, 4)),
               when = 1:4)
expect_equivalent(as.matrix(p + 1), as.matrix(2:5))

p <- position(journal(instrument = c("a", "a", "b", "b"),
                      amount     = c(1, 1, 1, 1),
                      timestamp  = c(1, 2, 3, 4)),
               when = 1:4)
a <- array(c(1, 2, 2, 2, 0, 0, 1, 2), dim = c(4L, 2L))
expect_equivalent(as.matrix(p + 1), a + 1)
expect_equivalent(as.matrix(p + a), 2 * a)
