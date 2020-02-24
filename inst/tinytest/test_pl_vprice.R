## single trade, instrument unnamed
j <- journal(amount = 1, price = 20)

p <- suppressWarnings(pl(j))   ## NA
expect_true(is.na(p[[1]]$pl))
pl(j, vprice = 21)  ## 1

pl(j, vprice = c(A = 21))
pl(j, vprice = c(B = 21))
expect_error(pl(j, vprice = c(B = 19, A = 21)))

## TODO: make tests

## ## single trade, instrument named
## j <- journal(amount = 1,
##              price = 20,
##              instrument = "A")

## pl(j)
## pl(j, vprice = 21)
## pl(j, vprice = c(A = 21))
## pl(j, vprice = c(B = 21))
## pl(j, vprice = c(B = 21, A = 21))



## ## single trade, journal has timestamp
## j <- journal(amount = 1,
##              price = 20,
##              instrument = "A",
##              timestamp = 5)

## pl(j)
## pl(j, vprice = 21)
## pl(j, vprice = c(A = 21))
## pl(j, vprice = c(B = 21))
## pl(j, vprice = c(B = 21, A = 21))



## ## single trade, along.timestamp is TRUE
## j <- journal(amount = 1,
##              price = 20,
##              instrument = "A",
##              timestamp = 5)

## pl(j, along.timestamp = TRUE)
## pl(j, along.timestamp = TRUE, vprice = 21)  ## FIXME INCORRECT: profit is labelled realised



##
j <- journal(amount = 1,
             price = 20,
             instrument = "A",
             timestamp = 5)

expect_error(pl(j, along.timestamp = 4:6)) ## should fail: vprice must be specified
p <- pl(j, along.timestamp = 4:6, vprice = c(21,20,22))
expect_equivalent(p[[1]]$pl, c(0,0,2))
expect_equivalent(p[[1]]$realised[-1], c(0,0))
expect_equivalent(p[[1]]$unrealised[-1], c(0,2))



##
J <- journal(instrument = c("A", "B", "B"),
             amount = c(1, 1, -1),
             price = c(100, 10, 11),
             timestamp = c(1, 1, 2))

P <- cbind(c(100, 101, 105),
           c(10, 12, 9))
colnames(P) <- c("A", "B")

p <- pl(J,
        along.timestamp = 1:3,
        vprice = P)

expect_equal(length(p), 2)
expect_equivalent(p[[1]]$pl,         c(0,1,5))
expect_equivalent(p[[1]]$realised,   c(0,0,0))
expect_equivalent(p[[1]]$unrealised, c(0,1,5))
expect_equivalent(p[[2]]$pl,         c(0,1,1))
expect_equivalent(p[[2]]$realised,   c(0,1,1))
expect_equivalent(p[[2]]$unrealised, c(0,0,0))

## switch columns
P <- cbind(c(10, 12, 9),
           c(100, 101, 105))
colnames(P) <- c("B", "A")
p <- pl(J,
        along.timestamp = 1:3,
        vprice = P)

expect_equal(length(p), 2)
expect_equivalent(p[[1]]$pl,         c(0,1,5))
expect_equivalent(p[[1]]$realised,   c(0,0,0))
expect_equivalent(p[[1]]$unrealised, c(0,1,5))
expect_equivalent(p[[2]]$pl,         c(0,1,1))
expect_equivalent(p[[2]]$realised,   c(0,1,1))
expect_equivalent(p[[2]]$unrealised, c(0,0,0))

## only single instrument
P <- cbind(c(10, 12, 9),
           c(100, 101, 105))
colnames(P) <- c("B", "A")
p <- pl(J[1],
        along.timestamp = 1:3,
        vprice = P)

expect_equal(length(p), 1)
expect_equivalent(p[[1]]$pl,         c(0,1,5))
expect_equivalent(p[[1]]$realised,   c(0,0,0))
expect_equivalent(p[[1]]$unrealised, c(0,1,5))

## only single instrument, 2
P <- cbind(c(10, 12, 9),
           c(100, 101, 105))
colnames(P) <- c("B", "A")
p <- pl(J[2:3],
        along.timestamp = 1:3,
        vprice = P)

expect_equal(length(p), 1)
expect_equivalent(p[[1]]$pl,         c(0,1,1))
expect_equivalent(p[[1]]$realised,   c(0,1,1))
expect_equivalent(p[[1]]$unrealised, c(0,0,0))


## do.sum
p <- pl(J,
        along.timestamp = 1:3,
        vprice = P, do.sum = TRUE)

expect_equivalent(p[[1]]$pl,         c(0,2,6))
expect_equivalent(p[[1]]$realised,   c(0,1,1))
expect_equivalent(p[[1]]$unrealised, c(0,1,5))
expect_equal(length(p), 1)




## PL at completely different times
j <- journal(amount = c(1, -1),
             price = 1:2,
             timestamp = 1:2)

res <- pl(j, along.timestamp = 6:10, vprice = 6:10)
expect_equivalent(res[[1]]$pl, rep(1, 5))
expect_equivalent(res[[1]]$realised, rep(1, 5))
expect_equivalent(res[[1]]$unrealised, rep(0, 5))

## TODO res <- pl(j, along.timestamp = -5:-1, vprice = 1:5)

j <- journal(amount = c(1, -1, 1, -1),
             price = c(1, 2, 1, 2),
             timestamp = c(0.1,0.2,0.3,0.4))

res <- pl(j, along.timestamp = 1:5, vprice = 1:5)
expect_equivalent(res[[1]]$pl, rep(2, 5))
expect_equivalent(res[[1]]$realised, rep(2, 5))
expect_equivalent(res[[1]]$unrealised, rep(0, 5))

j <- journal(amount = c(1, -1, 1, -1),
             price = c(1, 2, 1, 2),
             timestamp = 0) ## all trades mapped to
                            ## same timestamp

res <- pl(j, along.timestamp = 1:5, vprice = 1:5)
expect_equivalent(res[[1]]$pl, rep(2, 5))
expect_equivalent(res[[1]]$realised, rep(2, 5))
expect_equivalent(res[[1]]$unrealised, rep(0, 5))
expect_equivalent(res[[1]]$volume, rep(4, 5))
