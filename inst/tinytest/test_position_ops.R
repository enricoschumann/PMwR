## operations: +, -, /, ...

p1 <- position(c(a = 1, b = 2))
p2 <- position(c(b = 2, c = 3))

expect_equal(p1 + p2,
             position(c(a = 1, b = 4, c = 3)))

expect_equal(p1 - p2,
             position(c(a = 1, b = 0, c = -3)))

expect_equal(-p1,
             position(c(a = -1, b =-2)))

expect_equal(+p1, p1)



## unary !
p1 <- position(c(a = 1, b = 0, c = 1))
expect_equivalent(drop(!!p1),
                  c(TRUE, FALSE, TRUE))
