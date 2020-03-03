prices <- 1:10
expect_equal(
    length(journal(btest(prices = prices,
                         signal = function() NULL,
                         initial.position = 1,
                         prices0 = 1,
                         b = 0))),
    0) ## => there should be no trade at all
