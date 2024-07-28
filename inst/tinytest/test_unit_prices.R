cf <- data.frame(timestamp = c(as.Date("2017-1-1"),
                               as.Date("2017-1-5")),
                 cashflow = c(100, 100))

NAV <- data.frame(timestamp = seq(as.Date("2017-1-1"),
                                  as.Date("2017-1-10"),
                                  by = "1 day"),
                  NAV = c(100,101:103,204:209))

x1 <- unit_prices(NAV, cf, cf.included = TRUE)

#### now assume that the external flows have not been
#### included in the days NAV
NAV <- data.frame(timestamp = seq(as.Date("2017-1-1"),
                                  as.Date("2017-1-10"),
                                  by = "1 day"),
                  NAV = c(0,101:104,205:209))

x2 <- unit_prices(NAV, cf, cf.included = FALSE)

expect_equal(x1$price, x2$price)


####

NAV <- data.frame(1:3,
                  c(100,100,200))
cf <- data.frame(3, 100)
res <- unit_prices(NAV, cf, cf.included = TRUE, initial.price = 200)
expect_equivalent(res$units, c(0,0,0.5))


NAV <- data.frame(1:3,
                  c(100,101,101))
cf <- data.frame(1, 100)
(res <- unit_prices(NAV, cf, cf.included = TRUE, initial.price = 100))
expect_equivalent(res$units, c(1,1,1))
expect_equivalent(res$price, c(100,101,101))


####

NAV <- data.frame(1:3, c(100,100,200))
cf <- data.frame(t = c(1,1,3,3),
                 cf = c(80,20,50,50),
                 id = c(1,2,1,2))
unit_prices(NAV, cf, cf.included = TRUE, initial.price = 100)

NAV <- data.frame(1:3, c(100,100,100))
cf <- data.frame(t = c(1,1,3,3),
                 cf = c(80,20,-80,80),
                 id = c(1,2,1,2))
unit_prices(NAV, cf, cf.included = TRUE, initial.price = 100)






NAV <- data.frame(timestamp = 1:3,
                  NAV = c(100, 100, 105))
cf <- data.frame(timestamp = 1,
                 amount = 100)
res <- unit_prices(NAV,
                   cashflows = cf, initial.price = 100)
expect_equivalent(res$units, c(1,1,1))
expect_equivalent(res$price, c(100,100,105))

res <- unit_prices(NAV,
                   cashflows = cf, initial.price = 1000)
expect_equivalent(res$units, c(.1,.1,.1))
expect_equivalent(res$price, c(1000,1000,1050))

## --
## two cashflows on two different timestamps
NAV <- data.frame(timestamp = 1:3,
                  NAV = c(100, 200, 204))
cf <- data.frame(timestamp = 1:2,
                 amount = c(100, 100))
res <- unit_prices(NAV,
                   cashflows = cf, initial.price = 100)
expect_equivalent(res$units, c(1,2,2))
expect_equivalent(res$price, c(100,100,102))

## --
## two cashflows on two different timestamps: sell
NAV <- data.frame(timestamp = 1:3,
                  NAV = c(100, 200, 100))
cf <- data.frame(timestamp = 1:3,
                 amount = c(100, 100, -100))
res <- unit_prices(NAV,
                   cashflows = cf, initial.price = 100)
expect_equivalent(res$units, c(1,2,1))
expect_equivalent(res$price, c(100,100,100))

## --
## two cashflows on two different timestamps: sell to 0
NAV <- data.frame(timestamp = 1:3,
                  NAV = c(100, 200, 000))
cf <- data.frame(timestamp = 1:3,
                 amount = c(100, 100, -200))
res <- unit_prices(NAV,
                   cashflows = cf, initial.price = 100)
expect_equivalent(res$units, c(1,2,0))
expect_equivalent(res$price, c(100,100,100))


## --
##
NAV <- data.frame(timestamp = 1:3,
                  NAV = c(180, 185, 190))
cf <- data.frame(timestamp = 1,
                 amount = 100)
res <- unit_prices(NAV,
                   cashflows = cf,
                   initial.price = 20,
                   initial.units = 4)
expect_equal(res$units, c(9, 9, 9))
expect_equivalent(returns(res$price),
                  returns(c(180, 185, 190)))


## --

NAV <- data.frame(timestamp = 1:3,
                  NAV = c(180, 185, 190))
cf <- data.frame(timestamp = c(1, 1),
                 amount = c(50, 50))
unit_prices(NAV,
            cashflows = cf,
            initial.price = 20,
            initial.units = 4)
expect_equal(res$units, c(9, 9, 9))
expect_equivalent(returns(res$price),
                  returns(c(180, 185, 190)))
## --

NAV <- data.frame(timestamp = 1:3,
                  NAV = c(181, 285, 290))
cf <- data.frame(timestamp = c(1, 2),
                 amount = c(100, 100))
res <- unit_prices(NAV,
                   cashflows = cf,
                   initial.price = 20,
                   initial.units = 4)

p1 <- 81/4
x1 <- c(100/p1 + 4)
p2 <- 185/x1
x2 <- c(100/p2 + x1)
p3 <- 290/x2
expect_equivalent(res$price, c(p1, p2, p3))
expect_equivalent(res$units, c(x1, x2, x2))


## --

NAV <- data.frame(timestamp = 1:3,
                  NAV = c(180, 200, 202))
cf <- data.frame(timestamp = c(1, 2),
                 amount = c(100, 20))
res <- unit_prices(NAV,
                   cashflows = cf,
                   initial.price = 20,
                   initial.units = 4)
attr(res, "transactions")
expect_equivalent(res$price, c(20, 20, 202/200*20))
expect_equivalent(res$units, c(9, 10, 10))

## -- rounding

NAV <- data.frame(timestamp = 1,
                  NAV = 150)
cf <- data.frame(timestamp = 1, amount = 55)
res <- unit_prices(NAV = NAV, cashflows = cf,
                   initial.units = 100)
attr(res, "transactions")

res.r <- unit_prices(NAV = NAV, cashflows = cf,
                     initial.units = 100,
                     round.units = 4)
attr(res.r, "transactions")
new.units <- 55/(95/100)
expect_equivalent(res.r$units, round(100+new.units, 4))
expect_equivalent(attr(res.r, "transactions")$units, round(new.units, 4))
expect_equivalent(attr(res  , "transactions")$units,       new.units)
