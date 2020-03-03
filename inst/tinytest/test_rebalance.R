## WITHOUT NAMES

current <- c(0,0,100,100)
prices  <- c(1,1,1,1)
target  <- c(0.25, 0.25, 0.25, 0.25)

## missing names should raise error
expect_error(rebalance(current, target, prices))

x <- rebalance(current, target, prices,
               match.names = FALSE)
expect_equal(x$target, rep(50, 4))

x <- rebalance(current, target, prices,
               multiplier = 10,
               match.names = FALSE)

### ... no initial position: 'current' is 0
current <- 0
target  <- c(0.25, 0.25, 0.25, 0.25)
x <- rebalance(current, target, prices,
               match.names = FALSE, notional = 100)
expect_equal(x$target, rep(25, 4))

x <- rebalance(current, target, prices,
               match.names = FALSE, notional = 200)
expect_equal(x$target, rep(50, 4))

expect_error(
    rebalance(current, target, prices,  ## current is 0, so
              match.names = FALSE))     ## notional must be specified

### ... liquidate all: 'target' is 0
current <- c(1,1,1,1)
x <- rebalance(current, target = 0, prices,
               match.names = FALSE)
expect_equal(x$target, rep(0, 4))

current <- c(0,0,-1,-1)
x <- rebalance(current, target = 0, prices,
               match.names = FALSE)
expect_equal(x$target, rep(0,4))


### ... no position and move to target weight
x <- rebalance(current = 0, target = 0.25, prices,
               match.names = FALSE, notional = 100)
expect_equal(x$target, rep(25, 4))
expect_equal(x$difference, rep(25, 4))

x <- rebalance(current = 0, target = 0.25, prices,
               match.names = FALSE, notional = 1000)
expect_equal(x$target, rep(250, 4))
expect_equal(x$difference, rep(250, 4))

expect_error( ## target has 2 assets; prices has 4 assets
    rebalance(current = 0, target = c(0.5,0.5),
              prices, match.names = FALSE, notional = 100))




## WITH NAMES (match.names == TRUE is default)

prices  <- c(1,1,1,1)
names(prices) <- letters[1:4]

current <- c(b = 10)
target  <- c(d = 0.1)

x <- rebalance(current, target, prices)
expect_equal(x$target, c(0,1))

prices <- c(A = 1, B = 2, C = 3)
x <- rebalance(current = 0,
               target = 0.33,
               price = prices,
               notional = 100)
expect_equal(x$target, c(33, 16, 11))
expect_equal(x$target, x$difference)

prices <- c(A = 1, B = 2, C = 3)
x <- rebalance(current = 0,
               target = 0.1,
               price = prices,
               notional = 100)
expect_equal(x$target, c(10, 5, 3))
expect_equal(x$target, x$difference)



##  with position/journal
j <- journal(amount = c(1, 2),
             instrument = c("A", "B"))
w <- c(A = 0.5, B = 0.5)

amount <- rebalance(position(j), w, price = c(A = 1, B = 12))
expect_equal(as.journal(amount),
             structure(list(instrument = c("A", "B"),
                            timestamp = c(NA, NA),
                            amount = c(11, -1),
                            price = c(1, 12)),
                       .Names = c("instrument", "timestamp",
                                  "amount", "price"),
                       class = "journal"))


expect_equal(as.journal(amount, price = FALSE),
             structure(list(instrument = c("A", "B"),
                            timestamp  = c(NA, NA),
                            amount     = c(11, -1),
                            price      = c(NA, NA)),
                       .Names = c("instrument", "timestamp",
                                  "amount", "price"),
                       class = "journal"))

##  with two positions
prices  <- c(1,1,1,1)
names(prices) <- letters[1:4]

current <- position(amount = 10, instrument = "b")
target  <- position(amount = 5,  instrument = "d")

x <- rebalance(current, target, prices)
expect_equal(x$target, c(0,5))
expect_equal(x$difference, c(-10,5))

current <- position(amount = c(10,5), instrument = c("a", "b"))
target  <- position(amount = c(0,2), instrument = c("a", "b"))
prices  <- c(1,1)

x <- rebalance(current, target, prices, match.names=FALSE)
expect_equal(x$target, c(0,2))
expect_equal(x$difference, c(-10,-3))

## price <- c(a = 1, b = 2, c = 3)
## current <- c(a = 100, b = 20)
## target <- c(a = 0.2, c = 0.3)
## rebalance(current, target, price)

## price <- c(1,2,3)
## current <- c(100, 20, 0)
## target <- c(0.2, 0, 0.3)
## rebalance(current, target, price, match.names = FALSE)

## library("PMwR")

## j <- journal(amount = c(1, 2),
##              instrument = c("A", "B"),
##              price = c(1, 10))


## w <- c(A = 0.5, B = 0.5)
## (x <- rebalance(position(j), target=w, price = c(A=2, B =12)))
## journal(x)


## ins <- attr(position(j), "instrument")

## pos <- as.numeric(position(j))
## names(pos) <- attr(position(j), "instrument")

## rebalance(pos, target=w, price = c(A=2, B =12))

## j <- journal(amount = c(1, 2),
##              price = c(1, 10))
## w <- c(0.5)
## amount <- rebalance(position(j), w, price = 1, match.names=FALSE)

## j <- journal(amount = c(1, 2),
##              instrument = c("A", "B"),
##              price = c(1, 10))
## w <- c(A = 0.5, B = 0.5)

## amount <- rebalance(position(j), w, price = c(A=1, B=12))
## journal(amount)
## dput(journal(amount))
