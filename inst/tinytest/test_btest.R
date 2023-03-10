library("zoo", quietly = TRUE, warn.conflicts = FALSE)

btTable <- function(solution, prices)
    data.frame(prices = prices,
               position     = solution$position,
               suggested    = solution$suggested.position,
               wealth = solution$wealth,
               cash   = solution$cash)

prices <- c(100,98,98,97,101,102,101,98,99,101)

## signal returns NULL: not trade at all
signal <- function()
    NULL
solution <- btest(prices = prices, signal = signal)
expect_equal(drop(solution$position), rep(0, length(prices)))
expect_equal(drop(solution$wealth), rep(0, length(prices)))
expect_equal(solution$journal, journal())

## ... initial wealth not zero
solution <- btest(prices = prices, signal = signal, initial.cash = 100)
expect_equal(drop(solution$position), rep(0, length(prices)))
expect_equal(drop(solution$wealth), rep(100, length(prices)))
expect_equal(solution$journal, journal())

## ... initial position not zero
solution <- suppressWarnings( ## suppress "no ‘prices0’" warning
    btest(prices = prices, signal = signal, initial.position = 2))
expect_equal(drop(solution$position), rep(2, length(prices)))
expect_equal(drop(solution$wealth), prices*2)
expect_equal(solution$journal, journal())

## signal returns 1: hold one unit of asset
signal <- function()
    1

## ... default settings
solution <- btest(prices = prices, signal = signal)
expect_equal(unname(solution$position),
            structure(c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(length(prices), 1L)))
expect_equal(solution$wealth,
            c(0, 0, 0, -1, 3, 4, 3, 0, 1, 3))

## ... with no burn-in
solution <- btest(prices = prices, signal = signal, b = 0)
expect_equal(unname(solution$position),
            structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(length(prices), 1L)))
expect_equal(solution$wealth,
            c(0, -2, -2, -3, 1, 2, 1, -2, -1, 1))

## signal returns a weight
signal <- function()
    0.12

## ... no position (since wealth is 0)
solution <- suppressWarnings(btest(prices = prices,
                                   signal = signal,
                                   convert.weights = TRUE))
expect_equal(drop(solution$position), rep(0, length(prices)))
expect_equal(drop(solution$wealth), rep(0, length(prices)))
expect_equal(drop(solution$suggested.position), rep(0, length(prices)))
expect_equal(solution$journal, journal())


solution <- btest(prices = prices, signal = signal, convert.weights = TRUE,
                  initial.cash = 1000)
expect_equal((solution$wealth * signal()/prices)[-length(prices)],
            solution$suggested.position[-1])
expect_equal((solution$wealth * signal()/prices)[-length(prices)],
            solution$position[-1])


## signal returns a weight, 2 assets
prices2 <- cbind(A = prices, B = prices/2)
signal <- function()
    c(0.2, 0.3)

## ... no initial wealth, no position (creates a warning)
solution <- suppressWarnings(btest(list(prices2),
                                   signal = signal,
                                   convert.weights = TRUE))
expect_equal(dim(solution$position), dim(prices2))
expect_true(all(solution$position == 0))
expect_equal(solution$journal, journal())

## ... with initial wealth
solution <- btest(list(prices2), signal = signal,
                  convert.weights = TRUE,
                  initial.cash = 1000)
expect_equal((outer(solution$wealth, signal())/prices2)[-nrow(prices2), ],
            solution$position[-1L, ])

## ... with rebalancing in only 2 period
do.rebalance <- function()
    if (Time() == 3L || Time() == 8L)
        TRUE else FALSE
solution <- btest(list(prices2), signal = signal, convert.weights = TRUE,
                  initial.cash = 1000, do.rebalance = do.rebalance)
expect_equal(solution$position,
            structure(c(0, 0, 0,
                        2.04081632653061, 2.04081632653061,
                        2.04081632653061, 2.04081632653061,
                        2.04081632653061, 2.0512286547272,
                        2.0512286547272, 0, 0, 0,
                        6.12244897959184,
                        6.12244897959184, 6.12244897959184,
                        6.12244897959184, 6.12244897959184,
                        6.15368596418159, 6.15368596418159),
                      .Dim = c(10L, 2L), .Dimnames = list(NULL, c("A", "B"))))
expect_equal(c(signal()*solution$wealth[3]/prices2[3,]),
            c(solution$position[4,]))
expect_equal(c(signal()*solution$wealth[3]/prices2[3,]),
            c(solution$position[5,]))
expect_equal(c(signal()*solution$wealth[3]/prices2[3,]),
            c(solution$position[6,]))
expect_equal(c(signal()*solution$wealth[3]/prices2[3,]),
            c(solution$position[7,]))
expect_equal(c(signal()*solution$wealth[3]/prices2[3,]),
            c(solution$position[8,]))
expect_equal(c(signal()*solution$wealth[8]/prices2[8,]),
            c(solution$position[9,]))
expect_equal(c(signal()*solution$wealth[8]/prices2[8,]),
            c(solution$position[10,]))

## signal returns a weight, 3 assets
prices3 <- cbind(A = prices, B = prices/2, C = prices/3)
signal <- function()
    c(0.2, 0.3, 0.25)

## ... no initial wealth, no position (creates a warning)
solution <- suppressWarnings(btest(list(prices3),
                                   signal = signal,
                                   convert.weights = TRUE))
expect_equal(dim(solution$position), dim(prices3))
expect_true(all(solution$position == 0))
expect_equal(solution$journal, journal())

## ... with initial cash
solution <- btest(list(prices3), signal = signal,
                  convert.weights = TRUE,
                  initial.cash = 1000)
expect_equal((outer(solution$wealth, signal())/prices3)[-nrow(prices3), ],
            solution$position[-1L, ])



## tests for initial wealth
prices <- c(100,98,98,97,101,102,101,98,99,101)
signal <- function()
    0.5
solution <- btest(prices, signal = signal, convert.weights = TRUE,
                  initial.cash = 1000, b = 0, prices0 = 100)



## with timestamp
library("datetimeutils", quietly = TRUE)
timestamp <- seq(from = as.Date("2015-01-01"),
                 to   = as.Date("2015-04-15"),
                 by   = "1 day")
timestamp <- timestamp[!is_weekend(timestamp)]
## prices <- c(100+)

prices <- cbind(as.numeric(paste0("1.", format(timestamp, "%m%d"))),
                as.numeric(paste0("2.", format(timestamp, "%m%d"))))

res <- btest(list(prices), function() 1)
res$position
res$journal
res <- btest(list(prices), function() 1, b=0)
res$position
res$journal

res <- btest(list(prices),
             signal = function() c(0.5,0.5),
             convert.weights = TRUE,
             do.signal = "firstofmonth",
             initial.cash = 100,
             timestamp = timestamp)
res$journal

res <- btest(list(prices),
             signal = function() c(0.5,0.5),
             convert.weights = TRUE,
             do.signal = "lastofquarter",
             initial.cash = 100,
             timestamp = timestamp)
res$journal

res <- btest(list(prices),
             signal = function() c(0.5,0.5),
             convert.weights = TRUE,
             do.signal = "firstofquarter",
             initial.cash = 100,
             timestamp = timestamp)
res$journal

res <- btest(list(prices),
             signal = function() c(0.5,0.5),
             convert.weights = TRUE,
             do.rebalance = "lastofquarter",
             initial.cash = 100,
             timestamp = timestamp)
res$journal

res <- btest(list(prices),
             signal = function() c(0.5,0.5),
             convert.weights = TRUE,
             do.rebalance = "firstofquarter",
             initial.cash = 100,
             timestamp = timestamp)
res$journal




## check whether date is matched against
## timestamp. The 31 Jan 2015 is not in timestamp, so
## trade takes place on next day (2 Feb)
res <- btest(list(prices),
             signal = function() c(0.5,0.5),
             convert.weights = TRUE,
             do.signal = as.Date(c("2015-01-08",
                                   "2015-01-31")),
             initial.cash = 100,
             timestamp = timestamp)
expect_equal(unique(res$journal$timestamp),
            as.Date(c("2015-01-08", "2015-02-02")))
expect_equal(length(res$journal), 4)


## specifying when to trade
tmp <- structure(c(3490, 3458, 3434, 3358, 3287, 3321,
                   3419, 3535, 3589, 3603, 3626, 3677,
                   3672, 3689, 3646, 3633, 3631, 3599,
                   3517, 3549, 3572, 3578, 3598, 3634,
                   3618, 3680, 3669, 3640, 3675, 3604,
                   3492, 3513, 3495, 3503, 3497, 3433,
                   3356, 3256, 3067, 3228, 3182, 3286,
                   3279, 3269, 3182, 3205, 3272, 3185,
                   3201, 3236, 3272, 3224, 3194, 3188,
                   3213, 3255, 3261),
                 .Dim = c(57L, 1L),
                 .Dimnames = list(
                     NULL, "fesx201509"),
                 index = structure(
                     c(16617L, 16618L, 16619L, 16622L,
                       16623L, 16624L, 16625L, 16626L,
                       16629L, 16630L, 16631L, 16632L,
                       16633L, 16636L, 16637L, 16638L,
                       16639L, 16640L, 16643L, 16644L,
                       16645L, 16646L, 16647L, 16650L,
                       16651L, 16652L, 16653L, 16654L,
                       16657L, 16658L, 16659L, 16660L,
                       16661L, 16664L, 16665L, 16666L,
                       16667L, 16668L, 16671L, 16672L,
                       16673L, 16674L, 16675L, 16678L,
                       16679L, 16680L, 16681L, 16682L,
                       16685L, 16686L, 16687L, 16688L,
                       16689L, 16692L, 16693L, 16694L,
                       16695L), class = "Date"),
                 class = "zoo")

prices <- coredata(tmp)
timestamp <- index(tmp)

## have a position equal to the numeric timestamp
## => buy 1 unit in every period
signal <- function()
    Time(0)
res <- journal(btest(prices = prices, signal = signal))
expect_equal(length(res), length(tmp) - 1L)
expect_true(all(res$amount[-1] == 1))   ## the first traded amount
## is 2: first trade is at
## t==2, since the default
## lag of 1 is in force

expect_true(all(res$timestamp == seq(2, length(tmp))))

## buy at specified timestamps (integers; no lag!)
when <- c(10,20,30)
j <- journal(btest(prices = prices,
                   signal = signal,
                   do.signal = when))
expect_equal(j$timestamp, when)

## logical
j1 <- journal(btest(prices = prices, signal = signal,
                    do.signal = prices > 3600))
j2 <- journal(btest(prices = prices, signal = signal,
                    do.signal = function() Close(0L) > 3600))
expect_equal(j1, j2)


## do.rebalance FALSE -- strategy never trades
## promote warning to error
## options(warn=2)
## checkException({
##     options(warn=10);
##     btest(prices = prices, signal = signal,
##           do.signal = TRUE,
##           do.rebalance = FALSE)
## })
## warning only, no trades
options(warn = 0)
expect_equal(length(journal(suppressWarnings(btest(prices = prices, signal = signal,
                                                  do.signal = TRUE,
                                                  do.rebalance = FALSE)))),
            0L)

when <- c(10,20)
j <- journal(btest(prices = prices,
                   signal = signal,
                   do.rebalance = when))
expect_equal(j$timestamp, when)


## keywords
j <- journal(btest(prices = prices,
                   signal = signal,
                   do.signal = "firstofmonth",
                   timestamp = timestamp,
                   b = 0))

expect_equal(j$timestamp,
            PMwR:::first(timestamp, format(timestamp, "%Y-%m")))

j <- journal(btest(prices = prices,
                   signal = signal,
                   do.signal = "lastofmonth",
                   timestamp = timestamp))
expect_equal(j$timestamp,
            PMwR:::last(timestamp, format(timestamp, "%Y-%m")))


## include.data
prices <- c(100,98,98,97,101,102,101,98,99,101)
signal <- function()
    1
res <- btest(prices, signal = signal)
expect_equal(res$prices, NULL)
expect_equal(res$signal, NULL)
res <- btest(list(prices), signal = signal, include.data = TRUE)
expect_equal(res$prices, prices)
expect_equal(body(res$signal), body(signal))
## !is.list(prices) && is.null(dim(prices))

prices <- c(100,98,98,97,101,102,101,98,99,101)
prices <- cbind(prices, prices)
res <- btest(list(prices), signal = signal, include.data = TRUE)
expect_equal(res$prices, prices)








## ------------- lags
prices <- 101:110
signal <- function()
    if (Close() >= 104)
        1

btest(prices, signal,          b=3)$journal
btest(prices, signal, lag = 0, b=3)$journal
btest(prices, signal, lag = 2, b=3)$journal


expect_equal(
    journal(btest(
        1:10,
        signal = function() 1,
        b = as.Date("2018-1-5"),
        timestamp = as.Date("2018-1-1")+0:9))$timestamp,
    as.Date("2018-1-6"))

