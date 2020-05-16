pos <- position(c(A = 10, B = 5))
expect_equivalent(
    c(valuation(pos, price.table = t(c(2, 1)))), c(20, 5))



## -------------------------------------
## SINGLE timestamp
## -------------------------------------

## named position, named price.table
pos <- position(c(A = 10, B = 5))
v <- valuation(pos,
               price.table = c(B = 1, A = 5))
A <- array(c(50, 5), dim = c(1, 2))
expect_equivalent(unclass(v), A)

## named position, named price.table
pos <- position(c(A = 10, B = 5))
v <- valuation(pos,
               price.table = c(D = 99, B = 1, A = 5)) ## irrelevant price D
A <- array(c(50, 5), dim = c(1, 2))
expect_equivalent(unclass(v), A)

## named position, named price.table
pos <- position(c(A = 10, B = 5))
v <- valuation(pos,
               price.table = c(D = 99, B = 1),
               do.warn = FALSE) ## no price for A
A <- array(c(NA, 5), dim = c(1, 2))
expect_equivalent(unclass(v), A)

## named position, named price.table
pos <- position(c(A = 10, B = 5))
v <- valuation(pos,
               price.table = c(B = 1),
               do.warn = FALSE) ## no price for A
A <- array(c(NA, 5), dim = c(1, 2))
expect_equivalent(unclass(v), A)

##
pos <- position(c(A = 10, B = 5, X = 5)) ## position in 'X' without price
v <- valuation(pos,
               price.table = c(D = 99, B = 1, A = 5),
               do.warn = FALSE) ## irrelevant price D
A <- array(c(50, 5, NA), dim = c(1, 3))
expect_equivalent(unclass(v), A)



## -------------------------------------
## MULTIPLE timestamps
## -------------------------------------

pos <- A <- array(1:6, dim = c(3,2))
attr(pos, "timestamp") <- 1:3
attr(pos, "instrument") <- c("A", "B")
class(pos) <- "position"

P <- cbind(1:3, 1:3)

v <- valuation(pos, price.table = P)
expect_equivalent(v, A*P)

v <- valuation(pos, price.table = P, do.sum = TRUE)
expect_equivalent(c(v), rowSums(A*P))
