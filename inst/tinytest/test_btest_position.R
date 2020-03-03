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
