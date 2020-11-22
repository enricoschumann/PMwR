cf <- data.frame(timestamp = c(as.Date("2017-1-1"),
                               as.Date("2017-1-5")),
                 cashflow = c(100, 100))

NAV <- data.frame(timestamp = seq(as.Date("2017-1-1"),
                                  as.Date("2017-1-10"),
                                  by = "1 day"),
                  NAV = c(100,101:103,204:209))

x1 <- unit_prices(NAV, cf, cf.included = TRUE)

NAV <- data.frame(timestamp = seq(as.Date("2017-1-1"),
                                  as.Date("2017-1-10"),
                                  by = "1 day"),
                  NAV = c(0,101:104,205:209))

x2 <- unit_prices(NAV, cf, cf.included = FALSE)

expect_equal(x1$price,x2$price)


####

NAV <- data.frame(1:3,
                  c(100,100,200))
cf <- data.frame(3, 100)
unit_prices(NAV, cf, cf.included = TRUE, initial.price = 200)


NAV <- data.frame(1:3,
                  c(101,101,101))
cf <- data.frame(1, 100)
unit_prices(NAV, cf, cf.included = TRUE, initial.price = 100)


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
