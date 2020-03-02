## -*- truncate-lines: t; -*-


## test.split_trades <- function() {

##     amount <- c(1, -1)
##     price <- c(1, 2)
##     ans <- split_trades(amount, price, seq_along(amount))
##     checkEquals(length(ans), 1)
##     checkEquals(ans,
##                 list(structure(list(amount = c(1, -1),
##                                     price = c(1, 2),
##                                     timestamp = 1:2),
##                                .Names = c("amount", "price", "timestamp"))))



##     amount <- c(1, -2, 1)
##     price <- c(1,2,3)
##     ans <- split_trades(amount, price, seq_along(amount))
##     checkEquals(length(ans), 2)

##     checkEquals(ans[[1L]],
##                 structure(list(amount = c(1, -1),
##                                price = c(1, 2),
##                                timestamp = 1:2),
##                           .Names = c("amount", "price", "timestamp")))
##     checkEquals(ans[[2L]],
##                 structure(list(amount = c(-1, 1),
##                                price = c(2, 3),
##                                timestamp = 2:3),
##                           .Names = c("amount", "price", "timestamp")))

##     n <- c(1,1,-3,1)
##     p <- c(1,2,3,2)
##     tradetimes <- seq_along(n)
##     ans <- split_trades(n, p, tradetimes)
##     checkEquals(length(ans), 2)

## }


## test.btest <- function() {

##     library("zoo", quietly = TRUE, warn.conflicts = FALSE)

##     btTable <- function(solution, prices)
##         data.frame(prices = prices,
##                    position     = solution$position,
##                    suggested    = solution$suggested.position,
##                    wealth = solution$wealth,
##                    cash   = solution$cash)

##     prices <- c(100,98,98,97,101,102,101,98,99,101)

##     ## signal returns NULL: not trade at all
##     signal <- function()
##         NULL
##     solution <- btest(prices = prices, signal = signal)
##     checkEquals(drop(solution$position), rep(0, length(prices)))
##     checkEquals(drop(solution$wealth), rep(0, length(prices)))
##     checkEquals(solution$journal, journal())

##     ## ... initial wealth not zero
##     solution <- btest(prices = prices, signal = signal, initial.cash = 100)
##     checkEquals(drop(solution$position), rep(0, length(prices)))
##     checkEquals(drop(solution$wealth), rep(100, length(prices)))
##     checkEquals(solution$journal, journal())

##     ## ... initial position not zero
##     solution <- suppressWarnings( ## suppress "no ‘prices0’" warning
##         btest(prices = prices, signal = signal, initial.position = 2))
##     checkEquals(drop(solution$position), rep(2, length(prices)))
##     checkEquals(drop(solution$wealth), prices*2)
##     checkEquals(solution$journal, journal())

##     ## signal returns 1: hold one unit of asset
##     signal <- function()
##         1

##     ## ... default settings
##     solution <- btest(prices = prices, signal = signal)
##     checkEquals(unname(solution$position),
##                 structure(c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(length(prices), 1L)))
##     checkEquals(solution$wealth,
##                 c(0, 0, 0, -1, 3, 4, 3, 0, 1, 3))

##     ## ... with no burnin
##     solution <- btest(prices = prices, signal = signal, b = 0)
##     checkEquals(unname(solution$position),
##                 structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(length(prices), 1L)))
##     checkEquals(solution$wealth,
##                 c(0, -2, -2, -3, 1, 2, 1, -2, -1, 1))

##     ## signal returns a weight
##     signal <- function()
##         0.12

##     ## ... no position (since wealth is 0)
##     solution <- suppressWarnings(btest(prices = prices,
##                                        signal = signal,
##                                        convert.weights = TRUE))
##     checkEquals(drop(solution$position), rep(0, length(prices)))
##     checkEquals(drop(solution$wealth), rep(0, length(prices)))
##     checkEquals(drop(solution$suggested.position), rep(0, length(prices)))
##     checkEquals(solution$journal, journal())


##     solution <- btest(prices = prices, signal = signal, convert.weights = TRUE,
##                       initial.cash = 1000)
##     checkEquals((solution$wealth * signal()/prices)[-length(prices)],
##                 solution$suggested.position[-1])
##     checkEquals((solution$wealth * signal()/prices)[-length(prices)],
##                 solution$position[-1])


##     ## signal returns a weight, 2 assets
##     prices2 <- cbind(A = prices, B = prices/2)
##     signal <- function()
##         c(0.2, 0.3)

##     ## ... no initial wealth, no position (creates a warning)
##     solution <- suppressWarnings(btest(list(prices2),
##                                        signal = signal,
##                                        convert.weights = TRUE))
##     checkEquals(dim(solution$position), dim(prices2))
##     checkTrue(all(solution$position == 0))
##     checkEquals(solution$journal, journal())

##     ## ... with initial wealth
##     solution <- btest(list(prices2), signal = signal,
##                       convert.weights = TRUE,
##                       initial.cash = 1000)
##     checkEquals((outer(solution$wealth, signal())/prices2)[-nrow(prices2), ],
##                 solution$position[-1L, ])

##     ## ... with rebalancing in only 2 period
##     do.rebalance <- function()
##         if (Time() == 3L || Time() == 8L)
##             TRUE else FALSE
##     solution <- btest(list(prices2), signal = signal, convert.weights = TRUE,
##                       initial.cash = 1000, do.rebalance = do.rebalance)
##     checkEquals(solution$position,
##                 structure(c(0, 0, 0,
##                             2.04081632653061, 2.04081632653061,
##                             2.04081632653061, 2.04081632653061,
##                             2.04081632653061, 2.0512286547272,
##                             2.0512286547272, 0, 0, 0,
##                             6.12244897959184,
##                             6.12244897959184, 6.12244897959184,
##                             6.12244897959184, 6.12244897959184,
##                             6.15368596418159, 6.15368596418159),
##                           .Dim = c(10L, 2L), .Dimnames = list(NULL, c("A", "B"))))
##     checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
##                 c(solution$position[4,]))
##     checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
##                 c(solution$position[5,]))
##     checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
##                 c(solution$position[6,]))
##     checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
##                 c(solution$position[7,]))
##     checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
##                 c(solution$position[8,]))
##     checkEquals(c(signal()*solution$wealth[8]/prices2[8,]),
##                 c(solution$position[9,]))
##     checkEquals(c(signal()*solution$wealth[8]/prices2[8,]),
##                 c(solution$position[10,]))

##     ## signal returns a weight, 3 assets
##     prices3 <- cbind(A = prices, B = prices/2, C = prices/3)
##     signal <- function()
##         c(0.2, 0.3, 0.25)

##     ## ... no initial wealth, no position (creates a warning)
##     solution <- suppressWarnings(btest(list(prices3),
##                                        signal = signal,
##                                        convert.weights = TRUE))
##     checkEquals(dim(solution$position), dim(prices3))
##     checkTrue(all(solution$position == 0))
##     checkEquals(solution$journal, journal())

##     ## ... with initial cash
##     solution <- btest(list(prices3), signal = signal,
##                       convert.weights = TRUE,
##                       initial.cash = 1000)
##     checkEquals((outer(solution$wealth, signal())/prices3)[-nrow(prices3), ],
##                 solution$position[-1L, ])



##     ## tests for initial wealth
##     prices <- c(100,98,98,97,101,102,101,98,99,101)
##     signal <- function()
##         0.5
##     solution <- btest(prices, signal = signal, convert.weights = TRUE,
##                       initial.cash = 1000, b = 0, prices0 = 100)



##     ## with timestamp
##     library("datetimeutils", quietly = TRUE)
##     timestamp <- seq(from = as.Date("2015-01-01"),
##                      to   = as.Date("2015-04-15"),
##                      by   = "1 day")
##     timestamp <- timestamp[!is_weekend(timestamp)]
##     ## prices <- c(100+)

##     prices <- cbind(as.numeric(paste0("1.", format(timestamp, "%m%d"))),
##                     as.numeric(paste0("2.", format(timestamp, "%m%d"))))

##     res <- btest(list(prices), function() 1)
##     res$position
##     res$journal
##     res <- btest(list(prices), function() 1, b=0)
##     res$position
##     res$journal

##     res <- btest(list(prices),
##                  signal = function() c(0.5,0.5),
##                  convert.weights = TRUE,
##                  do.signal = "firstofmonth",
##                  initial.cash = 100,
##                  timestamp = timestamp)
##     res$journal

##     res <- btest(list(prices),
##                  signal = function() c(0.5,0.5),
##                  convert.weights = TRUE,
##                  do.signal = "lastofquarter",
##                  initial.cash = 100,
##                  timestamp = timestamp)
##     res$journal


##     ## check whether date is matched against
##     ## timestamp. The 31 Jan 2015 is not in timestamp, so
##     ## trade takes place on next day (2 Feb)
##     res <- btest(list(prices),
##                  signal = function() c(0.5,0.5),
##                  convert.weights = TRUE,
##                  do.signal = as.Date(c("2015-01-08",
##                                        "2015-01-31")),
##                  initial.cash = 100,
##                  timestamp = timestamp)
##     checkEquals(unique(res$journal$timestamp),
##                 as.Date(c("2015-01-08", "2015-02-02")))
##     checkEquals(length(res$journal), 4)


##     ## specifying when to trade
##     tmp <- structure(c(3490, 3458, 3434, 3358, 3287, 3321,
##                        3419, 3535, 3589, 3603, 3626, 3677,
##                        3672, 3689, 3646, 3633, 3631, 3599,
##                        3517, 3549, 3572, 3578, 3598, 3634,
##                        3618, 3680, 3669, 3640, 3675, 3604,
##                        3492, 3513, 3495, 3503, 3497, 3433,
##                        3356, 3256, 3067, 3228, 3182, 3286,
##                        3279, 3269, 3182, 3205, 3272, 3185,
##                        3201, 3236, 3272, 3224, 3194, 3188,
##                        3213, 3255, 3261),
##                      .Dim = c(57L, 1L),
##                      .Dimnames = list(
##                          NULL, "fesx201509"),
##                      index = structure(
##                          c(16617L, 16618L, 16619L, 16622L,
##                            16623L, 16624L, 16625L, 16626L,
##                            16629L, 16630L, 16631L, 16632L,
##                            16633L, 16636L, 16637L, 16638L,
##                            16639L, 16640L, 16643L, 16644L,
##                            16645L, 16646L, 16647L, 16650L,
##                            16651L, 16652L, 16653L, 16654L,
##                            16657L, 16658L, 16659L, 16660L,
##                            16661L, 16664L, 16665L, 16666L,
##                            16667L, 16668L, 16671L, 16672L,
##                            16673L, 16674L, 16675L, 16678L,
##                            16679L, 16680L, 16681L, 16682L,
##                            16685L, 16686L, 16687L, 16688L,
##                            16689L, 16692L, 16693L, 16694L,
##                            16695L), class = "Date"),
##                      class = "zoo")

##     prices <- coredata(tmp)
##     timestamp <- index(tmp)

##     ## have a position equal to the numeric timestamp
##     ## => buy 1 unit in every period
##     signal <- function()
##         Time(0)
##     res <- journal(btest(prices = prices, signal = signal))
##     checkEquals(length(res), length(tmp) - 1L)
##     checkTrue(all(res$amount[-1] == 1))   ## the first traded amount
##                                           ## is 2: first trade is at
##                                           ## t==2, since the default
##                                           ## lag of 1 is in force

##     checkTrue(all(res$timestamp == seq(2, length(tmp))))

##     ## buy at specified timestamps (integers; no lag!)
##     when <- c(10,20,30)
##     j <- journal(btest(prices = prices,
##                        signal = signal,
##                        do.signal = when))
##     checkEquals(j$timestamp, when)

##     ## logical
##     j1 <- journal(btest(prices = prices, signal = signal,
##                        do.signal = prices > 3600))
##     j2 <- journal(btest(prices = prices, signal = signal,
##                        do.signal = function() Close(0L) > 3600))
##     checkEquals(j1, j2)


##     ## do.rebalance FALSE -- strategy never trades
##     ## promote warning to error
##     ## options(warn=2)
##     ## checkException({
##     ##     options(warn=10);
##     ##     btest(prices = prices, signal = signal,
##     ##           do.signal = TRUE,
##     ##           do.rebalance = FALSE)
##     ## })
##     ## warning only, no trades
##     options(warn = 0)
##     checkEquals(length(journal(suppressWarnings(btest(prices = prices, signal = signal,
##                               do.signal = TRUE,
##                               do.rebalance = FALSE)))),
##                 0L)

##     when <- c(10,20)
##     j <- journal(btest(prices = prices,
##                        signal = signal,
##                        do.rebalance = when))
##     checkEquals(j$timestamp, when)


##     ## keywords
##     j <- journal(btest(prices = prices,
##                        signal = signal,
##                        do.signal = "firstofmonth",
##                        timestamp = timestamp,
##                        b = 0))

##     checkEquals(j$timestamp,
##                 PMwR:::first(timestamp, format(timestamp, "%Y-%m")))

##     j <- journal(btest(prices = prices,
##                        signal = signal,
##                        do.signal = "lastofmonth",
##                        timestamp = timestamp))
##     checkEquals(j$timestamp,
##                 PMwR:::last(timestamp, format(timestamp, "%Y-%m")))


##     ## include.data
##     prices <- c(100,98,98,97,101,102,101,98,99,101)
##     signal <- function()
##         1
##     res <- btest(prices, signal = signal)
##     checkEquals(res$prices, NULL)
##     checkEquals(res$signal, NULL)
##     res <- btest(list(prices), signal = signal, include.data = TRUE)
##     checkEquals(res$prices, prices)
##     checkEquals(body(res$signal), body(signal))
##     ## !is.list(prices) && is.null(dim(prices))

##     prices <- c(100,98,98,97,101,102,101,98,99,101)
##     prices <- cbind(prices, prices)
##     res <- btest(list(prices), signal = signal, include.data = TRUE)
##     checkEquals(res$prices, prices)








##     ## lags
##     prices <- 101:110
##     signal <- function()
##         if (Close() >= 104)
##             1

##     btest(prices, signal,          b=3)$journal
##     btest(prices, signal, lag = 0, b=3)$journal
##     btest(prices, signal, lag = 2, b=3)$journal


##     checkEquals(
##         journal(btest(
##             1:10,
##             signal = function() 1,
##             b = as.Date("2018-1-5"),
##             timestamp = as.Date("2018-1-1")+0:9))$timestamp,
##         as.Date("2018-1-6"))

## }

## test.btest.prices <- function() {

##     prices <- 1:5
##     checkEquals(
##         btest(prices, signal = function() 1),
##         btest(as.matrix(prices), signal = function() 1))

##     checkEquals(
##         btest(prices, signal = function() 1),
##         btest(list(prices), signal = function() 1))

##     checkEquals(
##         btest(prices, signal = function() 1),
##         btest(list(as.matrix(prices)), signal = function() 1))

##     library("zoo")
##     bt1 <- btest(prices, signal = function() 1)
##     bt2 <- btest(zoo(prices), signal = function() 1)
##     checkEqualsNumeric(bt1$wealth, bt2$wealth)
##     checkEqualsNumeric(bt1$position, bt2$position)
##     checkEqualsNumeric(bt1$suggested.position,
##                        bt2$suggested.position)

## }

## test.btest.b <- function() {
##     prices <- 1:5
##     timestamp <- Sys.Date() + 0:4

##     res <- btest(prices = 1:5,
##                  signal = function() 1,
##                  timestamp = timestamp,
##                  b = timestamp[1L] + 0.5)
##     checkEquals(res$b, 1)

##     res <- btest(prices = 1:5,
##                  signal = function() 1,
##                  timestamp = timestamp,
##                  b = timestamp[1L])
##     checkEquals(res$b, 1)

##     res <- btest(prices = 1:5,
##                  signal = function() 1,
##                  timestamp = timestamp,
##                  b = timestamp[1L] - 0.5)
##     checkEquals(res$b, 0)
## }

## test.btest.position <- function() {

##     ## single instrument
##     prices <- 1:10
##     for (i in 1:20) {
##         bt <- btest(prices,
##                     signal = function()
##                                  sample(0:10, 1, replace = TRUE))

##         checkEquals(unname(as.matrix(position(journal(bt),
##                                               when = 1:10))),
##                     unname(as.matrix(position(bt))))
##     }

##     ## two instruments
##     prices <- cbind(a = 1:10,
##                     b = 101:110)

##     for (i in 1:20) {
##         bt <- btest(list(prices),
##                     instrument = c("a", "b"),
##                     signal = function()
##                                  sample(0:10, 2, replace = TRUE))

##         checkEquals(unname(as.matrix(position(journal(bt),
##                                               when = 1:10))),
##                     unname(as.matrix(position(bt))))
##     }

## }

## test.btest.NA <- function() {

##     prices <- 1:10
##     signal <- function()
##         if (Time() < 5)
##             1 else 0
##     checkEquals(bt1 <- btest(prices, signal)$wealth,
##                 c(0, 0, 1, 2, 3, 4, 4, 4, 4, 4))

##     ## signal returns position
##     prices[7:10] <- NA
##     signal <- function()
##         if (Time() < 5)
##             1 else 0
##     checkEquals(bt2 <- btest(prices, signal)$wealth,
##                 c(0, 0, 1, 2, 3, 4, 4, 4, 4, 4))
##     checkEquals(bt1, bt2)

##     prices1  <- prices2 <- 1:10
##     prices2[7:10] <- NA
##     prices <- cbind(prices1, prices2)
##     signal <- function()
##         if (Time() < 5)
##             c(1,1) else c(1,0)
##     checkEquals(bt3 <- btest(list(prices), signal)$wealth,
##                 c(0, 0, 2, 4, 6, 8, 9, 10, 11, 12))


##     ## signal returns weight
##     prices <- 1:5
##     prices[4:5] <- NA
##     signal <- function() {
##         if (Time(0) <= 3)
##             0.5 else 0
##     }
##     btest(prices, signal, initial.cash = 100, convert.weights = TRUE)$wealth

##     prices1  <- prices2 <- 1:10
##     prices2[7:10] <- NA
##     prices <- cbind(prices1, prices2)
##     signal <- function()
##         if (Time() < 5)
##             c(1,1) else c(1,0)
##     checkEquals(bt3 <- btest(list(prices), signal)$wealth,
##                 c(0, 0, 2, 4, 6, 8, 9, 10, 11, 12))




## }

## test.btest.nullsignal <- function() {
##     prices <- 1:10

##     ## if signal returns NULL, the previous
##     ## position is kept.
##     signal1 <- function()
##         if (Time() == 5)
##             1 else Portfolio()
##     signal2 <- function()
##         if (Time() == 5)
##             1
##     signal3 <- function()
##         if (Time() == 5)
##             1 else NULL

##     checkEquals(btest(prices, signal1), btest(prices, signal2))
##     checkEquals(btest(prices, signal1), btest(prices, signal3))

## }

## test.btest.tc <- function() {
##     prices <- 1:10
##     signal <- function()
##         Time()
##     tc <- function()
##         Time() ## will be 0:9

##     journal(bt <- btest(prices, signal, tc = tc))
##     checkEquals(bt$cum.tc,
##                 c(0, cumsum(prices[-1]*seq_len(9))))
##                 ###                    ^^^^^^^^^^ tc

##     journal(bt <- btest(prices, signal, tc = 1))

##     checkEquals(bt$cum.tc,
##                 c(0, cumsum(prices[-1])))
## }

test.btest.journal <- function() {

    prices <- 1:10
    signal <- function()
        if (Time() < 5)
            1 else 0

    j <- journal(btest(prices, signal))
    checkEquals(j,
                journal(amount = c(1, -1),
                        timestamp = c(2, 6),
                        instrument = "asset 1",
                        price = c(2, 6)))

    j <- journal(btest(prices, signal, instrument = "A"))
    checkEquals(j,
                journal(amount = c(1, -1),
                        timestamp = c(2, 6),
                        instrument = "A",
                        price = c(2, 6)))
    checkEquals(j,
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
    checkEquals(j,
                journal(amount = c(1, 1, -1, -1),
                        timestamp = c(2, 2, 6, 6),
                        instrument = c("A", "B", "A", "B"),
                        price = c(2, 2.5, 6, 6.5)))

    j <- journal(btest(list(prices), signal,
                       ## overwrite instruments
                       instrument = c("a", "b")))
    checkEquals(j,
                journal(amount = c(1, 1, -1, -1),
                        timestamp = c(2, 2, 6, 6),
                        instrument = c("a", "b", "a", "b"),
                        price = c(2, 2.5, 6, 6.5)))

    signal <- function()
        if (Time() < 5L)
            c(0,1) else c(0,0)
    j <- journal(btest(list(prices), signal))
    checkEquals(j,
                journal(amount = c(1, -1),
                        timestamp = c(2, 6),
                        instrument = "B",
                        price = c(2.5, 6.5)))

}

test.btest.rc <- function() {

    prices <- 1:10
    prices <- cbind(A = prices, B = prices + 0.5)
    signal <- function()
        if (Time() < 5L)
            c(1,1) else c(0,0)
    bt <- btest(list(prices), signal,
                initial.cash = 100,
                instrument = colnames(prices),
                include.data = TRUE)
    weights <- cbind(prices, 1) *
        position(bt, include.cash = TRUE) / bt$wealth
    C <- rbind(0, returns(cbind(prices, 1)) * weights[-nrow(prices), ])
    checkEquals(rowSums(C), returns(bt$wealth, pad = 0))

}

test.btest.initial.position <- function() {
    prices <- 1:10
    checkEquals(
        length(journal(btest(prices = prices,
                             signal = function() NULL,
                             initial.position = 1,
                             prices0 = 1,
                             b = 0))),
        0) ## => there should be no trade at all
}

test.journal <- function() {

    ## a simple journal
    j <- journal(amount = 1:2)

    ## length
    checkEquals(length(j), 2)

    ## 'account' and 'id' missing: NULL
    checkTrue(is.null(j$account))
    checkTrue(is.null(j$id))

    ## 'timestamp', 'instrument' and 'price' missing: NA
    checkTrue(all(is.na(j$timestamp)))
    checkTrue(all(is.na(j$instrument)))
    checkTrue(all(is.na(j$price)))

    ## ... but length is same as length(amount)
    checkEquals(length(j$timestamp),  length(j))
    checkEquals(length(j$instrument), length(j))
    checkEquals(length(j$price),      length(j))

    ## empty journals
    checkEquals(journal(),
                structure(list(timestamp = numeric(0),
                               amount = numeric(0),
                               price = numeric(0),
                               instrument = character(0)),
                          .Names = c("timestamp", "amount",
                                     "price", "instrument"),
                          class = "journal"))

    ## FIXME as.journal.data.frame may create invalid journals
    ##       when there are no transactions
    ##
    ## checkEquals(as.journal(data.frame(amount = numeric(0),
    ##                                   comment = character(0),
    ##                                   stringsAsFactors = FALSE)),
    ##             structure(list(amount = numeric(0),
    ##                            comment = character(0)),
    ##                       .Names = c("amount", "comment"),
    ##                       class = "journal"))

    checkEquals(journal(),
                c(journal(), journal()))
    checkEquals(journal(),
                c(journal(), journal(), journal()))
    checkEquals(summary(journal()),
                structure(list(n_transactions = 0L, stats = NA),
                          .Names = c("n_transactions",
                                     "stats"),
                          class = "summary.journal"))


    ## a more reasonable journal
    timestamp <- 1:5
    amount <- 1
    price <- 101:105
    instrument <- "Stock A"
    j <- journal(timestamp = timestamp,
                 amount = amount,
                 price = price,
                 instrument = instrument)

    ## method: c
    checkEquals(c(j, journal()) , j)
    ## checkEquals(c(journal(), j), j)   ## TODO: sorting of fields

    ## subsetting
    ## j[1]
    checkEquals(j["stock"], j)
    checkEquals(length(j["bla"]), 0)

    ## method: c
    jj <- c(j, j)

    ## method: length
    checkEquals(length(jj), 10L)

    ## method: sort
    checkEquals(sort(jj)$timestamp, rep(1:5, each = 2))
    checkEquals(sort(jj, decreasing = TRUE)$timestamp, rep(5:1, each = 2))


    j <- journal(amount = 1:4,
                 instrument = c("a", "b", "a", "b"),
                 timestamp = c(1,1,2,2))
    checkEquals(sort(j)$timestamp, c(1,1,2,2))
    checkEquals(sort(j, decreasing = TRUE)$timestamp, c(2,2,1,1))

    sj <- sort(j, by = c("instrument", "timestamp"))
    checkEquals(sj$timestamp, c(1,2,1,2))
    checkEquals(sj$instrument, c("a", "a", "b", "b"))
    checkEquals(sj$amount, c(1,3,2,4))

    sj <- sort(j, by = c("instrument", "timestamp"),
               decreasing = TRUE)
    checkEquals(sj$timestamp, rev(c(1,2,1,2)))
    checkEquals(sj$instrument, rev(c("a", "a", "b", "b")))
    checkEquals(sj$amount, rev(c(1,3,2,4)))


    ## method: head/tail
    timestamp <- 1:20
    amount <- rep(1, length(timestamp))
    price <- sample(100:110, length(timestamp), replace = TRUE)
    instrument <- rep(letters[1:4], each = 5L)
    j <- journal(timestamp = timestamp, amount = amount,
                 price = price, instrument = instrument)
    head(j, 4)
    head(j, 4, by = FALSE)

    ## recycling
    checkEquals(journal(amount = 1, foo = 1:10),
                structure(list(instrument = c(NA_character_, NA_character_,
                                              NA_character_, NA_character_,
                                              NA_character_, NA_character_,
                                              NA_character_, NA_character_,
                                              NA_character_, NA_character_),
                               timestamp = c(NA, NA, NA, NA,
                                             NA, NA, NA, NA, NA, NA),
                               amount = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                               price = c(NA, NA, NA, NA, NA,
                                         NA, NA, NA, NA, NA),
                               foo = 1:10),
                          .Names = c("instrument", "timestamp",
                                     "amount", "price", "foo"),
                          class = "journal"))
    checkEquals(journal(amount = 1, foo = 1:10),
                journal(amount = rep(1, 10), foo = 1:10))

    checkEquals(journal(amount = 1:10, foo = 1),
                structure(list(instrument = c(NA_character_, NA_character_,
                                              NA_character_, NA_character_,
                                              NA_character_, NA_character_,
                                              NA_character_, NA_character_,
                                              NA_character_, NA_character_),
                               timestamp = c(NA, NA, NA, NA,
                                             NA, NA, NA, NA, NA, NA),
                               amount = 1:10,
                               price = c(NA, NA, NA, NA, NA,
                                         NA, NA, NA, NA, NA),
                               foo = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                          .Names = c("instrument", "timestamp",
                                     "amount", "price", "foo"),
                          class = "journal"))
    checkEquals(journal(amount = 1:10, foo = 1),
                journal(amount = 1:10, foo = rep(1, 10)))


    ## assignment
    j <- journal(amount=1:3)

    ## ok: replace field
    j$amount[1] <- 5   ## ok
    checkEquals(j$amount, c(5,2,3))

    ## not ok: replace journal as a whole
    checkException(j[1]$amount <- 10, silent = TRUE)

}

test.quote32 <- function() {

    checkEquals(q32("100-170"),
                structure(100.53125,
                          handle = 100,
                          ticks = 17,
                          fraction = 0,
                          class = "quote32"))

    checkEquals(q32("100-172"),
                structure(100.5390625,
                          handle = 100,
                          ticks = 17,
                          fraction = 1,
                          class = "quote32"))

    checkEquals(q32("100-175"),
                structure(100.546875,
                          handle = 100,
                          ticks = 17,
                          fraction = 2,
                          class = "quote32"))

    checkEquals(q32("100-177"),
                structure(100.5546875,
                          handle = 100,
                          ticks = 17,
                          fraction = 3,
                          class = "quote32"))


    checkEqualsNumeric(as.numeric(q32("109")),     109)
    checkEqualsNumeric(as.numeric(q32("109'00+")), 109 + 1/32/2)
    checkEqualsNumeric(as.numeric(q32("109'10")),  109 + 10/32)
    checkEqualsNumeric(as.numeric(q32("109-047")), 109+4.75/32)
    checkEquals(q32("127-00+"), q32("127-005"))

    checkEquals(q32("127:00+"), q32("127'005"))

}

test.rebalance <- function() {

    ## WITHOUT NAMES

    current <- c(0,0,100,100)
    prices  <- c(1,1,1,1)
    target  <- c(0.25, 0.25, 0.25, 0.25)

    ## missing names should raise error
    checkException(rebalance(current, target, prices),
                   silent = TRUE)

    x <- rebalance(current, target, prices,
                   match.names = FALSE)
    checkEquals(x$target, rep(50, 4))

    x <- rebalance(current, target, prices,
                   multiplier = 10,
                   match.names = FALSE)

    ### ... no initial position: 'current' is 0
    current <- 0
    target  <- c(0.25, 0.25, 0.25, 0.25)
    x <- rebalance(current, target, prices,
                   match.names = FALSE, notional = 100)
    checkEquals(x$target, rep(25, 4))

    x <- rebalance(current, target, prices,
                   match.names = FALSE, notional = 200)
    checkEquals(x$target, rep(50, 4))

    checkException(
        rebalance(current, target, prices,  ## current is 0, so
                  match.names = FALSE),     ## notional must be specified
        silent = TRUE)

    ### ... liquidate all: 'target' is 0
    current <- c(1,1,1,1)
    x <- rebalance(current, target = 0, prices,
                   match.names = FALSE)
    checkEquals(x$target, rep(0, 4))

    current <- c(0,0,-1,-1)
    x <- rebalance(current, target = 0, prices,
                   match.names = FALSE)
    checkEquals(x$target, rep(0,4))


    ### ... no position and move to target weight
    x <- rebalance(current = 0, target = 0.25, prices,
                   match.names = FALSE, notional = 100)
    checkEquals(x$target, rep(25, 4))
    checkEquals(x$difference, rep(25, 4))

    x <- rebalance(current = 0, target = 0.25, prices,
                   match.names = FALSE, notional = 1000)
    checkEquals(x$target, rep(250, 4))
    checkEquals(x$difference, rep(250, 4))

    checkException( ## target has 2 assets; prices has 4 assets
        rebalance(current = 0, target = c(0.5,0.5),
                  prices, match.names = FALSE, notional = 100),
        silent = TRUE)




    ## WITH NAMES (match.names == TRUE is default)

    prices  <- c(1,1,1,1)
    names(prices) <- letters[1:4]

    current <- c(b = 10)
    target  <- c(d = 0.1)

    x <- rebalance(current, target, prices)
    checkEquals(x$target, c(0,1))

    prices <- c(A = 1, B = 2, C = 3)
    x <- rebalance(current = 0,
                   target = 0.33,
                   price = prices,
                   notional = 100)
    checkEquals(x$target, c(33, 16, 11))
    checkEquals(x$target, x$difference)

    prices <- c(A = 1, B = 2, C = 3)
    x <- rebalance(current = 0,
              target = 0.1,
              price = prices,
              notional = 100)
    checkEquals(x$target, c(10, 5, 3))
    checkEquals(x$target, x$difference)



    ##  with position/journal
    j <- journal(amount = c(1, 2),
                 instrument = c("A", "B"))
    w <- c(A = 0.5, B = 0.5)

    amount <- rebalance(position(j), w, price = c(A = 1, B = 12))
    checkEquals(as.journal(amount),
                structure(list(instrument = c("A", "B"),
                               timestamp = c(NA, NA),
                               amount = c(11, -1),
                               price = c(1, 12)),
                          .Names = c("instrument", "timestamp",
                                     "amount", "price"),
                          class = "journal"))


    checkEquals(as.journal(amount, price = FALSE),
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
    checkEquals(x$target, c(0,5))
    checkEquals(x$difference, c(-10,5))

    current <- position(amount = c(10,5), instrument = c("a", "b"))
    target  <- position(amount = c(0,2), instrument = c("a", "b"))
    prices  <- c(1,1)

    x <- rebalance(current, target, prices, match.names=FALSE)
    checkEquals(x$target, c(0,2))
    checkEquals(x$difference, c(-10,-3))

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

}

test.as.journal <- function() {

    ## unnamed numeric vector
    checkEquals(journal(amount = 1:5),
                as.journal(1:5))

    ## named numeric vector
    x <- 1:3
    names(x) <- letters[1:3]
    checkEquals(journal(amount = 1:3,
                        instrument = letters[1:3]),
              as.journal(x))

    ## ERROR: no method for matrix
    x <- array(1, dim = c(2,2))
    checkException(as.journal(x), silent = TRUE)
}

test.returns <- function() {

    library("zoo", quietly = TRUE, warn.conflicts = FALSE)

    ## numeric vector
    x <- 101:112
    checkEqualsNumeric(returns(x), x[-1]/x[-length(x)]-1)
    checkEqualsNumeric(returns(x, pad = NA)[-1], x[-1]/x[-length(x)]-1)
    checkTrue(is.na(returns(x, pad = NA)[1]))


    ## numeric matrix
    x <- cbind(x,x)
    checkEqualsNumeric(returns(x), x[-1,]/x[-nrow(x),] - 1)
    checkEqualsNumeric(returns(x, pad = NA)[-1,], x[-1, ]/x[-nrow(x),]-1)
    checkTrue(all(is.na(returns(x, pad = NA)[1L,])))


    ## data.frame
    y <- as.data.frame(x)
    checkTrue(inherits(returns(y), "data.frame"))
    checkEquals(returns(y), y[-1,]/y[-nrow(x),] - 1)
    checkEqualsNumeric(as.matrix(returns(y, pad = NA)[-1,]), x[-1, ]/x[-nrow(x),]-1)
    checkTrue(all(is.na(returns(y, pad = NA)[1L,])))
    row.names(y) <- letters[1:nrow(y)]
    checkEquals(returns(y), y[-1,]/y[-nrow(x),] - 1)
    checkEquals(returns(x, pad = NA)[-1,], x[-1, ]/x[-nrow(x),]-1)


    ## lagged returns -- numeric vector
    x <- 101:112; lag <- 4
    checkEqualsNumeric(returns(x, lag = lag),
                       x[-(1:lag)]/x[-((length(x)-lag+1):length(x))]-1)
    checkEqualsNumeric(returns(x, pad = NA, lag = lag)[-(1:lag)],
                       x[-(1:lag)]/x[-((length(x)-lag+1):length(x))]-1)
    checkTrue(all(is.na(returns(x, pad = NA, lag = lag)[1:lag])))


    ## lagged returns -- matrix
    x <- cbind(x,x)
    checkEqualsNumeric(returns(x, lag = lag),
                       x[-(1:lag), ]/x[-((nrow(x)-lag+1):nrow(x)), ] - 1)
    checkEqualsNumeric(returns(x, pad = NA, lag = lag)[-(1:lag),],
                       x[-(1:lag), ]/x[-((nrow(x)-lag+1):nrow(x)), ] - 1)
    checkTrue(all(is.na(returns(x, pad = NA, lag = lag)[1:lag, ])))


    ## zoo -- numeric vector
    x <- 101:112
    z <- zoo(x, seq_along(x))
    checkEquals(returns(as.numeric(z)), returns(x))
    checkEquals(returns(as.numeric(z), pad = 0), returns(x, pad = 0))

    checkEquals(returns(z),
                zoo(returns(as.numeric(z)), index(z)[-1]))
    checkEquals(returns(z, pad = 0),
                zoo(returns(as.numeric(z), pad = 0), index(z)))


    ## padding in zoo -- numeric vector
    checkTrue(is.na(returns(z, pad = NA)[1L]))
    checkTrue(coredata(returns(z, pad = 0)[1L]) == 0)
    checkTrue(coredata(returns(z, pad = 1)[1L]) == 1)


    ## period, but no timestamp: period is ignored.
    ## timestamp, but no period: timestamp is ignored.
    ##
    ## (when there is no period/rebalance.when, methods
    ## are required to keep timestamp information for
    ## themselves and then to re-assemble the necessary
    ## class structure)
    x <- 101:112
    t <- seq_along(x)
    suppressWarnings(checkEquals(returns(x, period = "month"), returns(x)))
    suppressWarnings(checkEquals(returns(x, t = t),            returns(x)))

    ## period -- check class
    t <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "1 day")
    x <- seq_along(t)/10 + 100
    z <- zoo(x, t)
    ## z <- cbind(z,z,z)
    returns(z, period = "mtd")

    checkTrue("p_returns" %in% class(returns(x, t = t, period = "month")))
    checkTrue("p_returns" %in% class(returns(z,        period = "month")))
    checkTrue(class(returns(z)) == "zoo")

    ## period -- zoo or specify t
    checkEquals(returns(x, t = t, period = "month"),
                returns(z,        period = "month"))
    checkEquals(returns(x, t = t, period = "month", pad = NA),
                returns(z,        period = "month", pad = NA))

    ## as.zoo for p_returns
    ti <- match(aggregate(t, by = list(format(t, "%Y%m")), FUN = tail, 1)[[2]], t)
    checkEquals(as.zoo(returns(x, t = t, period = "month")),
                zoo(returns(x[c(1, ti)]), t[c(ti)]))

    ## period -- month end
    checkEquals(c(returns(x, t = t, period = "month", complete.first = FALSE)),
                returns(x[ti]))
    checkEquals(c(returns(x, t = t, period = "month", complete.first = TRUE)),
                returns(x[c(1,ti)]))
    checkEquals(c(returns(x, t = t, period = "month")),
                returns(x[c(1,ti)]))


    ## period -- ytd
    ## --> supress warning that 2012 is not the current year
    checkEquals(c(suppressWarnings(returns(x, t = t, period = "ytd"))),
                tail(x,1)/head(x,1) - 1)

    ## period -- mtd
    checkEquals(c(returns(x, t = t, period = "mtd")),
                tail(x, 1) / x[match(as.Date("2012-11-30"),t)] - 1)

    ## period -- quarterly
    checkEquals(c(returns(x, t = t, period = "quarterly")),
                returns(x[match(as.Date(c("2012-1-1",
                                          "2012-3-31",
                                          "2012-6-30",
                                          "2012-9-30",
                                          "2012-12-31")),t)]))



    ## from journal to time-weighted returns

    prices <- cbind(a = 101:110, b = 201:210)

    j <- journal(timestamp  = c(1,4,4,5,5,7),
                 amount     = c(1,1,1,-1,1,-1),
                 instrument = c("a", "a", "b", "a", "b", "a"),
                 price      = c(100.5,104.1,203,105,205.2,108))

    p <- position(j, when = 1:10)
    rowSums(p*prices)


    ## missing values
    x <- zoo(c(NA, 2:5), as.Date("2017-10-27") + 1:5)
    checkEqualsNumeric(unclass(returns(x, period = "month")), c(NA, 0.25))

}

test.returns.period <- function() {

    ## yearly returns
    library("datetimeutils", quietly = TRUE)

    dates <- sort(as.Date("1990-01-01") + sample(3700, 1000))
    x <- seq_along(dates)
    R <- returns(x, t = dates, period = "year")

    n <- nth_day(dates, period = "year", n = "last")
    ni <- nth_day(dates, period = "year", n = "last", index = TRUE)
    if (!1 %in% ni)
        ni <- c(1, ni)
    checkEqualsNumeric(.returns(x[ni], lag = 1), R)
    checkEquals(attr(R, "t"), n)

    R <- returns(x, t = dates, period = "year", complete.first = FALSE)
    n <- nth_day(dates, period = "year", n = "last")
    ni <- nth_day(dates, period = "year", n = "last", index = TRUE)
    checkEqualsNumeric(.returns(x[ni], lag = 1), R)
    checkEquals(attr(R, "t"), n[-1])


    checkEqualsNumeric(returns(rep(1, 10), period = "itd"), 0)
    checkEqualsNumeric(returns(rep(NA, 10), period = "itd"), NA_real_)

}

test.returns.rebalance  <- function() {

    prices <- cbind(a = 101:105, b = 201:205)

    ## 2 assets
    weights <- c(0.8, 0.2)
    ans <- returns(prices, weights = weights)
    checkEqualsNumeric(ans, returns(prices) %*% weights)

    weights <- c(0.8, 0.2)
    ans <- returns(prices, weights = weights,
                   rebalance.when = 1)
    checkEqualsNumeric(ans, returns(prices %*% (weights/prices[1, ])))

    weights <- c(0.8, 0.2)
    ans <- returns(prices, weights = weights,
                   rebalance.when = 2)
    tmp <- returns(prices %*% (weights/prices[2, ]))
    tmp[1] <- 0
    checkEqualsNumeric(ans, tmp)

    weights <- c(0.8, 0.2)
    ans <- returns(prices,
                   weights = weights,
                   rebalance.when = c(1, 3))
    tmp1 <- returns(prices[1:3, ] %*% (weights/prices[1L, ]))
    tmp2 <- returns(prices[3:5, ] %*% (weights/prices[3L, ]))
    checkEqualsNumeric(ans, c(tmp1, tmp2))

    weights <- c(0.8, 0.2)
    ans <- returns(prices,
                   weights = weights,
                   rebalance.when = FALSE)
    checkEqualsNumeric(ans, rep(0, nrow(prices)-1))

    weights <- rbind(c(0.8, 0.2),
                     c(0.5, 0.5))
    ans <- returns(prices,
                   weights = weights,
                   rebalance.when = c(1, 3))
    tmp1 <- returns(prices[1:3, ] %*% (weights[1L, ]/prices[1L, ]))
    tmp2 <- returns(prices[3:5, ] %*% (weights[2L, ]/prices[3L, ]))
    checkEqualsNumeric(ans, c(tmp1, tmp2))


    weights <- rbind(c(0.8, 0.2),
                     c(0.8, 0.2),
                     c(0.5, 0.5),
                     c(0.5, 0.5),
                     c(0.5, 0.5))
    ans <- returns(prices,
                   weights = weights,
                   rebalance.when = c(1, 3))
    tmp1 <- returns(prices[1:3, ] %*% (weights[1L, ]/prices[1L, ]))
    tmp2 <- returns(prices[3:5, ] %*% (weights[3L, ]/prices[3L, ]))
    checkEqualsNumeric(ans, c(tmp1, tmp2))



        ## portfolio returns with weights
    x <- 101:112
    t <- seq_along(x)
    x <- cbind(x+rnorm(length(x)), x+rnorm(length(x)))
    checkEquals(returns(x[,1]),
                c(returns(x, weights = c(1,0))))
    checkEquals(returns(x[,2]),
                c(returns(x, weights = c(0,1))))
    ## ... check attr
    checkEquals(length(attributes(returns(x, weights = c(0,1)))), 2)
    checkEquals(
        sort(names(attributes(returns(x, weights = c(1,0))))),
        c("contributions", "holdings"))


    ## ... with zoo
    checkEquals(returns(zoo(x,t))[,1],
                c(returns(zoo(x,t), weights = c(1,0))))
    checkEquals(returns(zoo(x,t))[,2],
                c(returns(zoo(x,t), weights = c(0,1))))

    ## ... check attr with zoo
    checkEquals(
        sort(names(attributes(returns(zoo(x,t), weights = c(1,0))))),
        c("class", "contributions", "holdings", "index"))

    ## ... match rebalance.when against timestamp
    h <- attr(returns(x, weights = c(0.2, 0.8), rebalance.when = 1),
              "holdings")
    checkTrue(all(apply(h, 2,
                        function(x) length(unique(x))) == 1L))

    h <- attr(returns(x, weights = c(0.2, 0.8), rebalance.when = 3),
              "holdings")
    checkTrue(all(apply(h, 2,
                        function(x) length(unique(x))) == 2L))

    x <- 101:110
    t <- as.Date("2017-1-1")+1:10
    x <- cbind(x + rnorm(length(x)),
               x + rnorm(length(x)))
    h1 <- attr(returns(x, t = t, weights = c(0.2, 0.8),
                      rebalance.when = as.Date("2017-1-4")),
              "holdings")
    checkTrue(all(apply(h1, 2,
                        function(x) length(unique(x))) == 2L))
    h2 <- attr(returns(zoo(x, t), weights = c(0.2, 0.8),
                      rebalance.when = as.Date("2017-1-4")),
              "holdings")
    checkTrue(all(apply(h2, 2,
                        function(x) length(unique(x))) == 2L))
    checkEquals(h1, h2)
    h3 <- attr(returns(x, t = t, weights = c(0.2, 0.8),
                      rebalance.when = 3),
              "holdings")
    checkEquals(h1, h3)
    h4 <- attr(returns(zoo(x, t), weights = c(0.2, 0.8),
                      rebalance.when = 3),
              "holdings")
    checkEquals(h1, h4)



    x  <- cumprod(1+rnorm(10, sd = 0.02))
    x2 <- cumprod(1+rnorm(10, sd = 0.02))
    X <- cbind(x, x2)

    checkEquals(
        tail(cumprod(1+returns(X, weights = c(.2,.8),
                               rebalance.when = 1)),
             1),
    c((X[nrow(X), ] / X[1, ]) %*% c(.2,.8)))

    ## time-weighted returns
    ## x <- 101:105
    ## checkEquals(returns(x, position = c(1, 1, 1, 1, 1)),
    ##             returns(x))
    ## checkEquals(returns(x, position = c(1, 1, 1, 1, 1), pad = NA),
    ##             returns(x, pad = NA))

    ## tmp <- returns(x)
    ## tmp[4] <- 0
    ## checkEquals(returns(x, position = c(1, 1, 1, 0, 0)),
    ##             tmp)

    ## checkEquals(returns(x, position = c(1,1,2,2,3)),
    ##             returns(x))
    ## checkEquals(returns(x, position = c(0,0,0,0,0)),
    ##             rep(0, 4))

    ## pos <- c(1,1,1,2,2,0)
    ## price <- c(100,100,100,100,100,100)
    ## dim(pos) <- dim(price) <- c(3, 2)
    ## checkEquals(returns(price, position = pos), returns(price[ ,1]))
    ## checkEquals(returns(price, position = pos),
    ##             rowSums((price*pos / rowSums(price*pos))[-3, ] * returns(price)))

    ## pos[ ,2] <- 0
    ## checkEquals(returns(price, position = pos),
    ##             returns(price[,1]))

    ## pos1 <- c(1,1,1,2,2,2)
    ## pos2 <- pos1 * 2
    ## price <- c(101,102,103,103,105,107)
    ## dim(price) <- dim(pos2) <- dim(pos1) <- c(3,2)

    ## checkEquals(returns(price, position = pos1),
    ##             rowSums((price*pos1 / rowSums(price*pos1))[-3, ] * returns(price)))
    ## checkEquals(returns(price, position = pos1),
    ##             returns(price, position = pos2))

}

test.returns.p_returns_monthly <- function() {

    library("zoo")
    t <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "1 day")
    x <- seq_along(t)/10 + 100
    z <- zoo(x, t)
    txt <- capture.output(returns(z, period = "month"))
    checkEquals("2012 3.0 2.8 2.9 2.7 2.8 2.6 2.6 2.6 2.4 2.4 2.3 2.3 36.5",
                txt[2])
    checkEquals(length(txt), 2)

    checkTrue(inherits(returns(x, t = t, period = "month"), "p_returns"))
    checkTrue(inherits(returns(x, t = t, period = "month"), "p_returns_monthly"))

    checkTrue(inherits(returns(z,        period = "month"), "p_returns"))
    checkTrue(inherits(returns(z,        period = "month"), "p_returns_monthly"))

}

test.returns.lag <- function() {
    x <- 1:10
    checkEquals(returns(x),
                returns(x, lag = 1))

    checkEquals(returns(x, lag = 2),
                x[3:10]/x[1:8] - 1)

    library("zoo")
    t <- as.Date("2000-1-1")+1:10
    checkEquals(returns(x, t, lag = 2),  ## 't' is ignored
                returns(x, lag = 2))
    checkEqualsNumeric(returns(zoo(x, t), lag = 2),
                       returns(x, lag = 2))

    checkEquals(index(returns(zoo(x, t), lag = 2)), t[-c(1:2)])

    checkEquals(index(returns(zoo(x, t), lag = 2, pad = NA)), t)

    checkTrue(all(is.na(
        coredata(returns(zoo(x, t), lag = 2, pad = NA))[1:2])))

}

test.scale1 <- function() {

    p <- c(104, 108, 104)
    checkEqualsNumeric(scale1(p), p/104)
    checkEqualsNumeric(scale1(p, when = 2), p/108)
    checkEquals(sd(returns(scale1(p, scale = TRUE))), 1)
    checkEquals(scale1(p, when = 2, scale = TRUE)[2L], 1)

    ## NA handling
    p <- cbind(c(104, 108, 104, 105),
               c( NA, 108, 104, 105))
    checkEqualsNumeric(scale1(p), p/108)
    checkEqualsNumeric(scale1(p, level = 100), p/1.08)

    p <- cbind(c(104, 108, 104, 105),
               c(103, 108, 104, 105))

    ## centring (aka de-meaning) -- arithmetic
    checkEquals(apply(returns(scale1(p, centre = TRUE, geometric=FALSE)),
                      2, mean, na.rm = TRUE),
                rep(0, ncol(p)))

    ## TODO: de-mean: geometric
    ## checkEquals(apply(returns(scale1(p, centre = TRUE, geometric=TRUE)),
    ##                   2, mean, na.rm = TRUE),
    ##             rep(0, ncol(p)))

    ## vol scaling -- target vol is 0.01
    checkEquals(apply(returns(scale1(p, scale = 0.01)),
                      2, sd, na.rm = TRUE),
                rep(0.01, ncol(p)))

    ## de-mean & scale -- arithmetic
    checkEquals(apply(returns(scale1(p,
                                     centre = TRUE,
                                     scale = 0.01,
                                     geometric=FALSE)),
                      2, mean, na.rm = TRUE),
                rep(0, ncol(p))) ## arith. mean is zero

    checkEquals(apply(returns(scale1(p,
                                     centre = TRUE,
                                     scale = 0.01,
                                     geometric=FALSE)),
                      2, sd, na.rm = TRUE),
                rep(0.01, ncol(p))) ## sd is 0.01

    ## de-mean & scale -- geometric
    P <- scale1(p, centre = TRUE,
                scale = 0.01,
                geometric = TRUE)
    checkEquals(P[nrow(P), ]/P[1, ],
                rep(1, ncol(p))) ## geom. mean is zero ==
                                 ## total return is zero


    ## de-meaning
    checkEquals(apply(returns(P), 2, sd, na.rm = TRUE),
                rep(0.01, ncol(p))) ## sd is 0.01



    P1 <- cumprod(1 + c(0, rnorm(20, sd = 0.02)))
    P1_scaled <- scale1(P1, centre = TRUE)

    checkEquals(sd(returns(P1)), sd(returns(P1_scaled)))
    checkEqualsNumeric(tail(P1_scaled,1), 1)

    P2 <- cumprod(1 + c(0, rnorm(20, sd = 0.02)))
    P2_scaled <- scale1(P2, centre = TRUE, scale = 0.03)

    checkEquals(sd(returns(P2_scaled)), 0.03)
    checkEquals(tail(P2_scaled,1),head(P2_scaled,1))

    ## ... should not affect correlation
    checkEquals(cor(returns(P1), returns(P2)),
                cor(returns(P1_scaled), returns(P2_scaled)))


    ## when: last, first
    P <- cumprod(1 + c(0, rnorm(20, sd = 0.02)))

    P.scaled <- scale1(P, when = "last", level = 42)
    checkEquals(returns(P), returns(P.scaled))
    checkEquals(P.scaled[length(P.scaled)], 42)

    P.scaled <- scale1(P, when = "first", level = 42)
    checkEquals(returns(P), returns(P.scaled))
    checkEquals(P.scaled[1], 42)

}

test.replace_weight <- function() {

    w <- c(a = 0.4, b = 0.6)
    a  <- c(x=0.9, y = 0.05, z = 0.05)
    b <- c(A=0.1, B = 0.2, C = 0.7)
    checkEquals(1,
                sum(replace_weight(w, a = a, b = b, x = c(aaa = 1))))

}

test.unit_prices  <- function() {

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

    checkEquals(x1$price,x2$price)

}

test.is_valid_ISIN <- function() {

    isin <- c("US0378331005",
              "AU0000XVGZA3",
              "DE000A0C3743",
              "not_an_isin")
    checkEquals(unname(is_valid_ISIN(isin)),
                c(TRUE, TRUE,  TRUE, FALSE))

    ## case is ignored
    checkEquals(unname(is_valid_ISIN(c("US0378331005",
                                       "us0378331005"))),
                c(TRUE, TRUE))

}

test.NAVseries <- function() {

    library("PMwR")
    library("RUnit")

    nav <- NAVseries(1:10)
    checkEquals(c(nav), 1:10)
    checkEquals(attr(nav, "timestamp"), 1:10)  ## integer timestamp added
    ## summary
    sum.nav <- summary(nav)[[1]]
    checkEquals(sum.nav$return, tail(nav,1)/head(nav, 1)-1)
    checkEquals(sum.nav$nna, 0)
    checkEquals(sum.nav$nobs, 10)



    ## with NA
    nav <- NAVseries(c(1, NA, 3:10))
    checkEquals(attr(nav, "timestamp"), 1:10)  ## integer timestamp added
    ## summary
    sum.nav <- summary(nav)[[1]]
    checkEquals(sum.nav$return, tail(nav,1)/head(nav, 1)-1)
    checkEquals(sum.nav$nna, 1)
    checkEquals(sum.nav$nobs, 10)


    ## with Date timestamp
    nav <- NAVseries(1:10, timestamp = as.Date("2017-1-1")+0:9)
    checkEquals(c(nav), 1:10)
    checkEquals(attr(nav, "timestamp"), as.Date("2017-1-1")+0:9)
    ## summary
    sum.nav <- summary(nav)[[1]]
    checkEquals(sum.nav$return, tail(nav,1)/head(nav, 1)-1)
    checkEquals(sum.nav$nna, 0)
    checkEquals(sum.nav$nobs, 10)

    ## scale1
    checkEqualsNumeric(scale1(NAVseries(10:15), level = 100),
                       NAVseries((10:15)*10))


    prices <- 100:109

    signal <- function()
        1
    bt <- btest(prices = prices, signal = signal, b = 0,
                initial.cash = 100)
    checkEquals(c(as.NAVseries(bt)), 100:109)

    ## summary(nav, monthly = FALSE)

    ## plot(NAVseries(1:10))
    ## plot(NAVseries(1:10, timestamp = as.Date("2017-1-1")+0:9))

    checkEquals(returns(as.NAVseries(bt)),
                returns(prices))


    ## leading NAs
    bt <- btest(prices = prices, signal = signal,
                b = 2, initial.cash = 100)

    ### ... no NAs
    checkEqualsNumeric(as.NAVseries(bt),
                       c(100, 100:107))

    ### ... one NA
    checkEqualsNumeric(as.NAVseries(bt, drop.NA = FALSE),
                       c(NA, 100, 100:107))

    ### ... several NAs
    bt <- btest(prices = prices, signal = signal,
                b = 5, initial.cash = 100)
    checkEqualsNumeric(as.NAVseries(bt, drop.NA = FALSE),
                       c(NA, NA, NA, NA, 100, 100, 101, 102, 103, 104))

}

test.NAVseries.summary <- function() {

    library("PMwR")
    library("RUnit")
    nav <- NAVseries(1:10)
    bm <- NAVseries(1:10)
    checkEquals(summary(nav)[[1]]$tracking.error, NULL)
    checkEquals(summary(nav, bm = bm)[[1]]$tracking.error, 0)

    nav <- NAVseries(cumprod(1+rnorm(10, sd = 0.01)))
    bm <- NAVseries(cumprod(1+rnorm(10, sd = 0.01)))
    checkEquals(summary(nav, bm = bm)[[1]]$tracking.error,
                sd(returns(nav) - returns(bm)))
    checkEquals(summary(nav, bm = bm)[[2]]$tracking.error, 0)
    checkEquals(summary(nav,
                        bm = bm,
                        assume.daily = TRUE)[[1]]$tracking.error,
                16*sd(returns(nav) - returns(bm)))

}

test.NAVseries.window <- function() {

    x <- NAVseries(101:110, 1:10)

    checkEquals(window(x, 2, 3),
                structure(102:103, timestamp = 2:3,
                          description = character(0),
                          class = "NAVseries"))
    checkEquals(window(x), x)
}

test.pricetable <- function() {

    checkTrue(all(pricetable(1:10) == 1:10))

    checkEquals(pricetable(1:10, timestamp = 1:10, instrument = "A"),
                pricetable(1:10, timestamp = 1:10, instrument = "A")[,"A"])

    checkEquals(pricetable(1:10, timestamp = 1:10, instrument = "A"),
                pricetable(1:10, timestamp = 1:10, instrument = "A")[1:10,"A"])

    checkEquals(pricetable(1:10, timestamp = 1:10, instrument = "A"),
                pricetable(1:10, timestamp = 1:10, instrument = "A")[1:10,])


    ## repeated column
    pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[ ,c("A","A")]
    checkEquals(attr(pt, "instrument"), c("A","A"))
    checkEquals(pt,
                structure(c(1L, 2L, 3L, 1L, 2L, 3L),
                          .Dim = c(3L, 2L),
                          timestamp = 1:3,
                          instrument = c("A", "A"),
                          class = "pricetable"))

    ## repeated column + NA column
    pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[ ,c("A","B","A")]
    checkEquals(attr(pt, "instrument"), c("A","B","A"))
    checkEquals(pt,
                structure(c(1L, 2L, 3L, NA, NA, NA, 1L, 2L, 3L),
                          .Dim = c(3L, 3L),
                          timestamp = 1:3,
                          instrument = c("A", "B", "A"),
                          class = "pricetable"))


    ## repeated column + NA column + NA rows
    pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[0:4 ,c("A","B","A")]
    checkEquals(pt,
                structure(c(NA, 1L, 2L, 3L, NA,
                            NA, NA, NA, NA, NA,
                            NA, 1L, 2L, 3L, NA),
                          .Dim = c(5L, 3L),
                          timestamp = 0:4,
                          instrument = c("A", "B", "A"),
                          class = "pricetable"))

    pt1 <- pricetable(1:3, timestamp = 1:3, instrument = "A")[0:4 ,c("A","B","A")]
    pt2 <- pricetable(1:3, timestamp = 1:3, instrument = "A")[0:4 ,c("A","B","A"), missing = -99]
    checkTrue(all(unclass(pt2)[is.na(pt1), drop = FALSE] == -99))


    pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[0:4 ,c("A","B","A"), missing = "locf"]
    checkEquals(pt,
                structure(c(NA, 1L, 2L, 3L, 3L,
                            NA, NA, NA, NA, NA,
                            NA, 1L, 2L, 3L, 3L),
                          .Dim = c(5L, 3L),
                          timestamp = 0:4,
                          instrument = c("A", "B", "A"),
                          class = "pricetable"))


    ## setting up a pt: vector => matrix
    checkTrue(all(dim(pricetable(1:2, instrument = c("A", "B"))) == c(1, 2)))
    checkTrue(all(dim(pricetable(1:2, timestamp = 1:2)) == c(2,1)))
    checkEquals(pricetable(1:2, instrument = c("A", "B")),
                pricetable(c(A = 1, B = 2)))

}
