## single instrument
prices <- 1:10
for (i in 1:20) {
    bt <- btest(prices,
                signal = function()
        sample(0:10, 1, replace = TRUE))

    expect_equal(unname(as.matrix(position(journal(bt),
                                           when = 1:10))),
                 unname(as.matrix(position(bt))))
}



## two instruments
prices <- cbind(a = 1:10,
                b = 101:110)

for (i in 1:20) {
    bt <- btest(list(prices),
                instrument = c("a", "b"),
                signal = function()
        sample(0:10, 2, replace = TRUE))

    expect_equal(unname(as.matrix(position(journal(bt),
                                           when = 1:10))),
                 unname(as.matrix(position(bt))))
}



## replicate wealth from position (amounts)
na <- 5
nt <- 10
prices <- rnorm(na*nt, sd = 0.01)
dim(prices) <- c(nt, na)
prices <- apply(prices, 2, function(x) cumprod(x + 1))

signal <- function() {
    n <- ncol(Close())
    sample(0:5, n, replace = TRUE)
}

bt <- btest(list(prices), signal = signal, b = 0)
p <- position(bt, include.cash = TRUE)
expect_equivalent(rowSums(p*cbind(prices, 1)), bt$wealth)



## replicate scaled wealth from weights
na <- 5
nt <- 10
prices <- rnorm(na*nt, sd = 0.01)
dim(prices) <- c(nt, na)
prices <- apply(prices, 2, function(x) cumprod(x + 1))

signal <- function() {
    n <- ncol(Close())
    sample(0:5, n, replace = TRUE)
}

bt <- btest(list(prices), signal = signal, b = 0, initial.cash = 100)
p <- position(bt, include.cash = TRUE)
weights <- (p*cbind(prices, 1)) / bt$wealth

r <- returns(cbind(prices, 1), pad = 0)

expect_equivalent(rowSums(weights[-nrow(weights), ] * r[-1, ]),
                  returns(bt$wealth))
