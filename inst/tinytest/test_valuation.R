pos <- position(c(A = 10, B = 5))
expect_equivalent(
    c(valuation(pos, vprice = t(c(2, 1)))), c(20, 5))



## -------------------------------------
## SINGLE timestamp
## -------------------------------------

## named position, named vprice
pos <- position(c(A = 10, B = 5))
v <- valuation(pos,
               vprice = c(B = 1, A = 5), use.names = TRUE)
A <- array(c(50, 5), dim = c(1, 2))
expect_equivalent(unclass(v), A)

## named position, named vprice, without use.names
pos <- position(c(A = 10, B = 5))
v <- valuation(pos,
               vprice = c(B = 1, A = 5), use.names = FALSE)
A <- array(c(50, 5), dim = c(1, 2))
expect_equivalent(c(unclass(v)), c(unclass(pos)) * c(1, 5))

## named position, named vprice
pos <- position(c(A = 10, B = 5))
v <- valuation(pos,
               vprice = c(D = 99, B = 1, A = 5),  ## irrelevant price D
               use.names = TRUE)
A <- array(c(50, 5), dim = c(1, 2))
expect_equivalent(unclass(v), A)

## named position, named vprice
pos <- position(c(A = 10, B = 5))
v <- valuation(pos,
               vprice = c(D = 99, B = 1),  ## no price for A
               do.warn = FALSE,
               use.names = TRUE)
A <- array(c(NA, 5), dim = c(1, 2))
expect_equivalent(unclass(v), A)

## named position, named vprice
pos <- position(c(A = 10, B = 5))
v <- valuation(pos,
               vprice = c(B = 1),  ## no price for A
               do.warn = FALSE,
               use.names = TRUE)
A <- array(c(NA, 5), dim = c(1, 2))
expect_equivalent(unclass(v), A)

##
pos <- position(c(A = 10, B = 5, X = 5)) ## position in 'X' without price
v <- valuation(pos,
               vprice = c(D = 99, B = 1, A = 5),  ## irrelevant price D
               do.warn = FALSE,
               use.names = TRUE)
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

v <- valuation(pos, vprice = P)
expect_equivalent(v, A*P)

v <- valuation(pos, vprice = P, do.sum = TRUE)
expect_equivalent(c(v), rowSums(A*P))


## -------------------------------------
## SINGLE timestamp, named prices
## -------------------------------------

pos <- A <- array(1:6, dim = c(3,2))
attr(pos, "timestamp") <- 1:3
attr(pos, "instrument") <- c("A", "B")
class(pos) <- "position"

P <- cbind(2*1:3, 1:3)
colnames(P) <- c("B", "A")
v <- valuation(pos, vprice = P, use.names = FALSE)
expect_equivalent(A * P, v)

v <- valuation(pos, vprice = P, use.names = TRUE)
expect_equivalent(A * P[, c("A", "B")], v)


## -------------------------------------
## MULTIPLE timestamps, named prices
## -------------------------------------

pos <- A <- array(1:6, dim = c(3,2))
attr(pos, "timestamp") <- 1:3
attr(pos, "instrument") <- c("A", "B")
class(pos) <- "position"

P <- cbind(2*1:3, 1:3)
colnames(P) <- c("B", "A")
v <- valuation(pos, vprice = P, use.names = FALSE)
expect_equivalent(A * P, v)

v <- valuation(pos, vprice = P, use.names = TRUE)
expect_equivalent(A * P[, c("A", "B")], v)
