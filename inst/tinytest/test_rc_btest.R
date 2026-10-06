prices <- 1:10
prices <- cbind(A = prices, B = prices + 0.5)
bm <- btest(list(prices),
            signal = function()
                if (Time() < 5L)
                    c(1, 1) else c(0, 0),
            initial.cash = 100,
            instrument = colnames(prices),
            include.data = TRUE)

w.bm <- position(bm, unit = "weight", include.cash = TRUE)

s <- btest(list(prices),
           signal = function()
               if (Time() > 5L)
                   c(1, 0) else c(0, 1),
           initial.cash = 100,
           instrument = colnames(prices),
           include.data = TRUE)

w <- position(s, unit = "weight", include.cash = TRUE)


R <- cbind(returns(prices), cash = 0)

rc(weights = w[-nrow(w), ],
   weights.bm = w.bm[-nrow(w), ],
   R = R, linking.method = "Carino1999")
