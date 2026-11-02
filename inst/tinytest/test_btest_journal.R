prices <- 1:10
signal <- function()
    if (Time() < 5)
        1 else 0

j <- journal(btest(prices, signal))
expect_equal(j,
             journal(amount = c(1, -1),
                     timestamp = c(2, 6),
                     instrument = "asset 1",
                     price = c(2, 6)))

j <- journal(btest(prices, signal, instrument = "A"))
expect_equal(j,
             journal(amount = c(1, -1),
                     timestamp = c(2, 6),
                     instrument = "A",
                     price = c(2, 6)))
expect_equal(j,
             journal(amount = c(1, -1),
                     timestamp = c(2, 6),
                     instrument = c("A", "A"),
                     price = c(2, 6)))

prices <- 1:10
prices <- cbind(A = prices, B = prices+0.5)
signal <- function()
    if (Time() < 5L)
        c(1,1) else c(0,0)
j <- journal(btest(list(prices), signal))
expect_equal(j,
             journal(amount = c(1, 1, -1, -1),
                     timestamp = c(2, 2, 6, 6),
                     instrument = c("A", "B", "A", "B"),
                     price = c(2, 2.5, 6, 6.5)))

j <- journal(btest(list(prices), signal,
                   ## overwrite instruments
                   instrument = c("a", "b")))
expect_equal(j,
             journal(amount = c(1, 1, -1, -1),
                     timestamp = c(2, 2, 6, 6),
                     instrument = c("a", "b", "a", "b"),
                     price = c(2, 2.5, 6, 6.5)))

signal <- function()
    if (Time() < 5L)
        c(0,1) else c(0,0)
j <- journal(btest(list(prices), signal))
expect_equal(j,
             journal(amount = c(1, -1),
                     timestamp = c(2, 6),
                     instrument = "B",
                     price = c(2.5, 6.5)))




## --- trade.at.open  and  journal() ---
P <- 1:20
dim(P) <- c(5, 4)
colnames(P) <- c("O", "H", "L", "C")

bt <- btest(P,
            signal = function()
                if (Time() >= 3) 1 else 0)
expect_equivalent(tail(bt$wealth, 1),
                  P[nrow(P), "C"] - P[4, "O"])
expect_equivalent(tail(bt$wealth, 1),
                  pl(pl(bt, vprice = P[nrow(P), "C"])))
expect_equivalent(journal(bt)$price, P[4, "O"])


bt <- btest(P,
            trade.at.open = FALSE,
            signal = function()
                if (Time() >= 3) 1 else 0)
expect_equivalent(tail(bt$wealth, 1),
                  P[nrow(P), "C"] - P[4, "C"])
expect_equivalent(tail(bt$wealth, 1),
                  pl(pl(bt, vprice = P[nrow(P), "C"])))
expect_equivalent(journal(bt)$price, P[4, "C"])




O <- rep(1:4, each = 5)
dim(O) <- c(5, 4)

C <- rep(11:14, each = 5)
dim(C) <- c(5, 4)


bt <- btest(list(O, O, O, C),
            signal = function()
                if (Time() >= 3) rep(1, 4) else 0)
expect_equivalent(tail(bt$wealth, 1),
                  sum(C[5, ]) - sum(O[4, ]))
expect_equivalent(journal(bt)$price, O[4, ])


bt <- btest(list(O, O, O, C),
            trade.at.open = FALSE,
            signal = function()
                if (Time() >= 3) rep(1, 4) else 0)
expect_equivalent(tail(bt$wealth, 1),
                  sum(C[5, ]) - sum(C[4, ]))
expect_equivalent(journal(bt)$price, C[4, ])
