## -*- truncate-lines: t; -*-

test.position <- function() {

    checkEqualsNumeric(position(amount = 1:5), 15)

    ## ERROR: at least 'amount' needs to be specified
    checkException(position(), silent = TRUE)

    
    ## Construct position from raw data or from journal
    t <- 1:5
    n <- c(1, 1, -1, 3, -4)
    j <- journal(timestamp = t, amount = n)

    ## ... 1) check correct position for *vector* input
    checkEquals(c(position(amount = n, timestamp = t, when = 4.9)),
                4)
    checkEquals(c(position(amount = n, timestamp = t, when = c(-10,1.4,4.9,10))),
                c(0, 1, 4, 0))

    ## ... 2) check correct position for *journal* input
    checkEquals(c(position(j, when = 4.9)), 4)
    checkEquals(c(position(j, when = c(-10,1.4,4.9,10))), c(0,1,4,0))

    ## ... 3) check equal output for *vector* and *journal* input
    checkEquals(position(amount = n, timestamp = t, when = 4.9),
                position(j, when = 4.9))
    checkEquals(position(amount = n, timestamp = t, when = c(-10,1.4,4.9,10)),
                position(j, when = c(-10,1.4,4.9,10)))

    
    ## Ops
    x <- position(amount = 1, instrument = c("a"))
    y <- position(amount = 1:2, instrument = c("a","b"))
    checkEquals(x, +x)
    checkEquals(y, +y)
    checkEquals(x+y,
                structure(c(2, 2),
                          .Dim = 1:2,
                          .Dimnames = list("1", c("a", "b")),
                          timestamp = 1,
                          instrument = c("a", "b"),
                          class = "position"))
    
}

test.splitTrades <- function() {
    amount <- c(1, -1)
    price <- c(1,2)
    ans <- splitTrades(amount, price, seq_along(amount))
    checkEquals(length(ans), 1)
    checkEquals(ans,
                list(structure(list(amount = c(1, -1),
                                    price = c(1, 2),
                                    timestamp = 1:2),
                               .Names = c("amount", "price", "timestamp"))))

    
    
    amount <- c(1, -2, 1)
    price <- c(1,2,3)
    ans <- splitTrades(amount, price, seq_along(amount))
    checkEquals(length(ans), 2)

    checkEquals(ans[[1L]],
                structure(list(amount = c(1, -1),
                               price = c(1, 2),
                               timestamp = 1:2),
                          .Names = c("amount", "price", "timestamp")))
    checkEquals(ans[[2L]],
                structure(list(amount = c(-1, 1),
                               price = c(2, 3),
                               timestamp = 2:3),
                          .Names = c("amount", "price", "timestamp")))
    
    n <- c(1,1,-3,1)
    p <- c(1,2,3,2)
    tradetimes <- seq_along(n)
    splitTrades(n,p,tradetimes)
}


test.btest <- function() {
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
    checkEquals(drop(solution$position), rep(0, length(prices)))
    checkEquals(drop(solution$wealth), rep(0, length(prices)))
    checkEquals(solution$journal, journal())

    ## ... initial wealth not zero
    solution <- btest(prices = prices, signal = signal, initial.cash = 100)
    checkEquals(drop(solution$position), rep(0, length(prices)))
    checkEquals(drop(solution$wealth), rep(100, length(prices)))
    checkEquals(solution$journal, journal())

    ## ... initial position not zero
    solution <- btest(prices = prices, signal = signal, initial.position = 2)
    checkEquals(drop(solution$position), rep(2, length(prices)))
    checkEquals(drop(solution$wealth), prices*2)
    checkEquals(solution$journal, journal())

    ## signal returns 1: hold one unit of asset
    signal <- function()
        1

    ## ... default settings
    solution <- btest(prices = prices, signal = signal)
    checkEquals(unname(solution$position),
                structure(c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(length(prices), 1L)))
    checkEquals(solution$wealth,
                c(0, 0, 0, -1, 3, 4, 3, 0, 1, 3))

    ## ... with no burnin
    solution <- btest(prices = prices, signal = signal, b = 0)
    checkEquals(unname(solution$position),
                structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(length(prices), 1L)))
    checkEquals(solution$wealth,
                c(0, -2, -2, -3, 1, 2, 1, -2, -1, 1))

    ## signal returns a weight
    signal <- function()
        0.12

    ## ... no position (since wealth is 0)
    solution <- btest(prices = prices, signal = signal, convert.weights = TRUE)
    checkEquals(drop(solution$position), rep(0, length(prices)))
    checkEquals(drop(solution$wealth), rep(0, length(prices)))
    checkEquals(drop(solution$suggested.position), rep(0, length(prices)))
    checkEquals(solution$journal, journal())
    

    solution <- btest(prices = prices, signal = signal, convert.weights = TRUE,
                      initial.cash = 1000)
    checkEquals((solution$wealth * signal()/prices)[-length(prices)],
                solution$suggested.position[-1]) 
    checkEquals((solution$wealth * signal()/prices)[-length(prices)],
                solution$position[-1]) 


    ## signal returns a weight, 2 assets
    prices2 <- cbind(A = prices, B = prices/2)
    signal <- function()
        c(0.2, 0.3)

    ## ... no initial wealth, no position
    solution <- btest(list(prices2), signal = signal, convert.weights = TRUE)
    checkEquals(dim(solution$position), dim(prices2))
    checkTrue(all(solution$position==0))
    checkEquals(solution$journal, journal())

    ## ... with initial wealth
    solution <- btest(list(prices2), signal = signal, convert.weights = TRUE,
                      initial.cash = 1000)
    checkEquals((outer(solution$wealth, signal())/prices2)[-nrow(prices2), ],
                solution$position[-1L, ])

    ## ... with rebalancing in only 2 period
    do.rebalance <- function()
        if (Time() == 3L || Time() == 8L)
            TRUE else FALSE
    solution <- btest(list(prices2), signal = signal, convert.weights = TRUE,
                      initial.cash = 1000, do.rebalance = do.rebalance)
    checkEquals(solution$position,
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
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[4,]))
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[5,]))
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[6,]))
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[7,]))
    checkEquals(c(signal()*solution$wealth[3]/prices2[3,]),
                c(solution$position[8,]))
    checkEquals(c(signal()*solution$wealth[8]/prices2[8,]),
                c(solution$position[9,]))
    checkEquals(c(signal()*solution$wealth[8]/prices2[8,]),
                c(solution$position[10,]))
    
    ## signal returns a weight, 3 assets
    prices3 <- cbind(A = prices, B = prices/2, C = prices/3)
    signal <- function()
        c(0.2, 0.3, 0.25)

    ## ... no initial wealth, no position
    solution <- btest(list(prices3), signal = signal, convert.weights = TRUE)
    checkEquals(dim(solution$position), dim(prices3))
    checkTrue(all(solution$position==0))
    checkEquals(solution$journal, journal())

    ## ... no initial wealth, no position
    solution <- btest(list(prices3), signal = signal, convert.weights = TRUE,
                      initial.cash = 1000)
    checkEquals((outer(solution$wealth, signal())/prices3)[-nrow(prices3), ],
                solution$position[-1L, ])



    ## tests for initial wealth
    prices <- c(100,98,98,97,101,102,101,98,99,101)
    signal <- function()
        0.5
    solution <- btest(prices, signal = signal, convert.weights = TRUE,
                      initial.cash = 1000, b = 0, prices0 = 100)



    ## with timestamp
    require("datetimeutils")
    timestamp <- seq(from = as.Date("2015-01-01"),
                     to   = as.Date("2015-04-15"),
                     by   = "1 day")
    timestamp <- timestamp[!isWeekend(timestamp)]
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

    ## check whether date is matched against
    ## timestamp. The 31 Jan is not in timestamp, so
    ## trade takes place on next day (2 Feb)    
    res <- btest(list(prices),
                 signal = function() c(0.5,0.5),
                 convert.weights = TRUE,
                 do.signal = as.Date(c("2015-01-08",
                                       "2015-01-31")),
                 initial.cash = 100,
                 timestamp = timestamp)
    checkEquals(unique(res$journal$timestamp),
                as.Date(c("2015-01-08", "2015-02-02")))    
    checkEquals(length(res$journal), 4)
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

    checkEquals(as.journal(data.frame(amount = numeric(0),
                                      comment = character(0),
                                      stringsAsFactors = FALSE)),
                structure(list(amount = numeric(0),
                               comment = character(0)),
                          .Names = c("amount", "comment"),
                          class = "journal"))

    
    ## a more reasonable journal
    timestamp <- 1:5
    amount <- 1
    price <- 101:105
    instrument <- "Stock A"
    j <- journal(timestamp = timestamp, amount = amount,
                 price=price, instrument = instrument)

    ## method: c
    checkEquals(c(j, journal()) , j)    
    ## checkEquals(c(journal(), j), j)   ## TODO: sorting of fields

    ## subsetting
    ## j[1]
    checkEquals(j["stock"], j)
    checkEquals(length(j["bla"]), 0)
    
    ## method: c
    jj <- c(j, j)

    ## method: sort
    checkEquals(sort(jj)$timestamp, rep(1:5, each = 2))
    checkEquals(sort(jj, decreasing = TRUE)$timestamp, rep(5:1, each = 2))
    
    ## method: length
    checkEquals(length(jj), 10L)

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
    checkException(j[1]$amount <- 10, silent=TRUE)

}

test.pl <- function() {

    checkEquals(pl(amount = c(1,-1),
                   price  = c(1,2))[[1]][["pl"]], 1)

    
    checkEquals(pl(amount = c(1,-1), price = c(1,2)),                
                structure(list(structure(list(pl = 1,
                                              realised = NA,
                                              unrealised = NA, 
                                              buy = 1,
                                              sell = 2,
                                              volume = 2),
                                         .Names = c("pl",
                                                    "realised", "unrealised",
                                                    "buy", "sell", "volume"))),
                          class = "pl", along.timestamp = FALSE, instrument = NA))
                
    checkEquals(pl(amount = 1, price = 1,
                   initial.position = 1, initial.price = 1,
                   vprice = 2),
                structure(list(structure(list(pl = 2,
                                              realised = NA,
                                              unrealised = NA, 
                                              buy = 1,
                                              sell = 2,
                                              volume = 1),
                                         .Names = c("pl",
                                                    "realised", "unrealised",
                                                    "buy", "sell", "volume"))),
                          class = "pl", along.timestamp = FALSE, instrument = NA))


    x <- pl(amount = c(1,-1, 20,-20),
            price = c(100,102, 4,7),
            instrument = c("Equity A", "Equity A", "Equity B", "Equity B"))

    checkEquals(x,
                structure(list(`Equity A` = structure(list(pl = 2,
                                                           realised = NA, 
                                                           unrealised = NA,
                                                           buy = 100, sell = 102,
                                                           volume = 2),
                                                      .Names = c("pl",
                                                                 "realised", "unrealised",
                                                                 "buy", "sell", "volume")),
                               `Equity B` = structure(list(pl = 60,
                                                           realised = NA,
                                                           unrealised = NA,
                                                           buy = 4, sell = 7, 
                                                           volume = 40),
                                                      .Names = c("pl",
                                                                 "realised", "unrealised",
                                                                 "buy", "sell", "volume"))),
                          class = "pl", along.timestamp = FALSE,
                          instrument = c("Equity A", 
                                         "Equity B"),
                          .Names = c("Equity A", "Equity B")))
                
    
    
    checkEquals(as.data.frame(x),
                structure(list(pl = c(2, 60),
                               buy = c(100, 4),
                               sell = c(102, 7),
                               volume = c(2, 40)),
                          .Names = c("pl", "buy",
                                     "sell", "volume"),
                          row.names = c("Equity A", "Equity B"),
                          class = "data.frame"))

    ## open positions and no vprice specified => pl is NA
    timestamp <- 1:4
    amount <- c(1,1,1,1)
    price <- 101:104
    instrument <- "Bond"
    jnl <- journal(timestamp=timestamp, amount=amount,
                   price=price, instrument=instrument)
    checkTrue(is.na(suppressWarnings(pl(jnl)[[1]][["pl"]])))



    ## initial position
    tmp <- pl(journal(),
              initial.position = 1,
              initial.price = 100,
              vprice = 105)
    checkEquals(tmp[[1]][["pl"]], 5)
    
    tmp <- pl(journal(),
              initial.position = c(A = 1, B = 2),
              initial.price = c(A = 100),
              vprice = c(A = 105, B = 110))
    checkEquals(tmp[[1]][["pl"]],5)
    checkTrue(is.na(tmp[[2]][["pl"]]))
    
    tmp <- pl(journal(),
              initial.position = c(A = 1, B = 2),
              initial.price = c(A = 100, B = 100),
              vprice = c(A = 105, B = 110))
    checkEquals(unlist(lapply(tmp, `[[`, "pl")), c(A=5,B=20))
    
    tmp <- pl(journal(),
              initial.position = c(A = 1, B = 2),
              initial.price = c(A = 100, B = 100),
              vprice = c(B = 110, A = 105))
    checkEquals(unlist(lapply(tmp, `[[`, "pl")), c(A=5,B=20))

    tmp <- pl(journal(), multiplier = 2,
              initial.position = c(A = 1, B = 2),
              initial.price = c(A = 100, B = 100),
              vprice = c(A = 105, B = 110))
    checkEquals(unlist(lapply(tmp, `[[`, "pl")), c(A=10,B=40))


    
    ## initial position can be a 'position'
    tmp <- pl(journal(),
              initial.position = position(amount = c(1,2),
                                          instrument = c("A", "B")),
              initial.price = c(A = 100, B = 100),
              vprice = c(A = 105, B = 110))
    checkEquals(unlist(lapply(tmp, `[[`, "pl")), c(A=5,B=20))

    tmp <- pl(journal(), 
              initial.position = position(amount = c(2,1),
                                          instrument = c("B", "A")),
              initial.price = c(A = 100, B = 100),
              vprice = c(A = 105, B = 110))
    checkEquals(unlist(lapply(tmp, `[[`, "pl")), c(A=5,B=20))

    
    ## initial position can be a 'journal'
    tmp <- pl(journal(),
              multiplier = c(A = 2,
                             B = 4),
              initial.position = journal(amount     = c(2,1),
                                  instrument = c("B", "A")),
              initial.price = c(A = 100,
                                B = 100),
              vprice = c(A = 105, B = 110))
    checkEquals(unlist(lapply(tmp, `[[`, "pl")), c(A=10,B=80))
    
    
    ## amount <- c(1,1,-1,1,-1)
    ## price <- c(100,99,101,100,101)
    ## pl(amount, price, along.timestamp = TRUE)
    ## PMwR:::avg(amount, price)
    
    ## amount <- c(1,-2,1)
    ## price <- c(100,101,100)
    ## pl(amount, price)
    ## pl(amount, price, along.timestamp = TRUE)
    ## PMwR:::avg(amount, price)

    ## tmp <- splitTrades(amount, price, timestamp = seq_along(amount),
    ##                    aggregate = TRUE)    
    ## PMwR:::avg(tmp$amount, tmp$price)

    
    ## J <- journal(timestamp = c(1, 2, 3),
    ##              amount = c(1, 1, -2),
    ##              price  = c(100,102, 101))
    ## pl(c(1, 1, -2), c(100,102, 101))



    ## pl(amount = 1, price = 1, vprice = 2)
    
    ## pl(amount = 1, price = 2, initial.position = 1,
    ##    initial.price = 1, vprice = 3)
    
    
    ## amount <- c(1,1,-1,1,-2)
    ## price <- c(100,102,105,102,105)
    
    ## cumcash <- cumsum(-price * amount)
    ## cumpos  <- cumsum(amount)
    ## pnl <- cumpos * price + cumcash
    ## real <- avg(amount, price)$realised
    ## unreal <- pnl-real
    ## data.frame(cumsum(amount), price, pnl, real, unreal)
    
    
    ## require("PMwR")
    ## require("RUnit")

    ## multiplier
    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2),
                   multiplier = 0)[[1L]][["pl"]], 0)
    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2),
                   multiplier = 1)[[1L]][["pl"]], 1)
    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2))[[1L]][["pl"]], 1)
    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2),
                   multiplier = 2)[[1]][["pl"]], 2)

    checkEquals(pl(amount = c(1, -1),
                   price  = c(1,  2),
                   instrument = c("B", "B"),
                   multiplier = c(A = 1, B = 2))[[1L]][["pl"]], 2)

    ## along.timestamp
    j <- journal(amount = c(1,-1),
                 timestamp = c(1,2.5),
                 price = c(100,101))
    pl(j, along.timestamp = TRUE)
    checkEqualsNumeric(pl(j, along.timestamp = TRUE)[[1]]$pl,
                       0:1)


    pl(j, along.timestamp=1:3, vprice = c(100,102,103))

    tmp <- pl(amount = 1, timestamp = 0, price = 100,
              vprice = 101:110, along.timestamp = 1:10)
    checkEqualsNumeric(tmp[[1]]$pl, 1:10)
    
    ## should work since vprice is timestamp-agnostic:
    ## it just computes the current PL, no matter the
    ## time
    pl(journal(),
       initial.position = 1, initial.price = 1,
       vprice = 101)

    ## should *not* work since the initial price has no
    ## timestamp, but for vprice the timestamps are
    ## specified
    checkException(
        pl(journal(),
           initial.position = 1, initial.price = 1,
           vprice = 101:110, along.timestamp = 1:10),
        silent = TRUE)


    
}

## pl(amount = 1, price = 1,
##    initial.position = 1, initial.price = 1,
##    vprice = 2)


## instrument  <- c("FGBL", "FGBL", "Bond", "Bond")
## amount <- c(1, -2, 2, -1)
## price <- c(100,101, 1, 5)
## ## .pl(amount, price)
## pl.default(amount, price, instrument=instrument)
## pl.default(amount, price, instrument=instrument, vprice=c(FGBL=103, Bond = 2))

## amount <- c(1, -2)
## price <- c(100,101)
## pl.default(amount, price)
## pl.default(amount, price, vprice=100)

## require("rbenchmark")

## amount <- rep(c(1,-1), times = 1000)
## price <- rep(c(100,101), times = 1000)

## benchmark(-drop(crossprod(amount, price)),
##           -c(crossprod(amount, price)),
##           -sum(amount * price),
##           columns = c("test", "elapsed", "relative"),
##           replications = 100000, order ="relative")


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

    
    
     ## q32("100-272") - q32("100-270")
     ## as.numeric(q32("100-272") - q32("100-270"))
     
    checkEqualsNumeric(as.numeric(q32("109-047")), 109+4.75/32)
    
}


test.rebalance <- function() {

    require(PMwR) ; require(RUnit)
    current <- c(0,0,100,100)
    prices  <- c(1,1,1,1)
    target  <- c(0.25, 0.25, 0.25, 0.25)
    x <- rebalance(current, target, prices, match.names = FALSE)
    checkEquals(x$target, rep(50, 4))

    
    ## no initial position: 'current' is 0
    current <- 0
    prices  <- c(1,1,1,2)
    target  <- c(0.25, 0.25, 0.25, 0.25)
    x <- rebalance(current, target, prices,
                   match.names = FALSE, notional = 100)
    checkEquals(x$target, c(rep(25,3), 12))

    
    ## liquidate all: 'target' is 0
    current <- c(0,0,100,100)
    x <- rebalance(current, target = 0, prices,
                   match.names = FALSE, notional = 100)
    checkEquals(x$target, rep(0,4))

    current <- c(0,0,-100,-100)
    x <- rebalance(current, target = 0, prices,
                   match.names = FALSE, notional = 100)
    checkEquals(x$target, rep(0,4))

    
    ## with names
    prices  <- c(1,1,1,1)
    names(prices) <- letters[1:4]

    current <- c(b = 10)
    target  <- c(d = 0.1)

    x <- rebalance(current, target, prices, match.names = TRUE)
    checkEquals(x$target, c(0,1))

    
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

## require("PMwR")

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

    require("zoo", quietly = TRUE, warn.conflicts = FALSE)

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

    
    ## period, but no timestamp: period is ignored
    ## timestamp, but no period: timestamp is ignored
    ##
    ## (when there is no period, methods are required
    ## to keep timestamp information for themselves and
    ## then to re-assemble the necessary class
    ## structure)
    x <- 101:112
    t <- seq_along(x)
    suppressWarnings(checkEquals(returns(x, period = "month"), returns(x)))
    suppressWarnings(checkEquals(returns(x, t = t),            returns(x)))
    
    ## period -- check class
    ## require("PMwR", quietly = TRUE)
    ## require("RUnit", quietly = TRUE)
    ## require("zoo", quietly = TRUE, warn.conflicts = FALSE)
    t <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "1 day")
    x <- seq_along(t)/10 + 100
    z <- zoo(x, t)
    ## z <- cbind(z,z,z)
    returns(z, period = "mtd")
    
    checkTrue(class(returns(x, t = t, period = "month")) == "p_returns")
    checkTrue(class(returns(z,        period = "month")) == "p_returns")
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
    checkEquals(c(returns(x, t = t, period = "ytd")),
                tail(x,1)/head(x,1) - 1)
    
    ## period -- mtd
    checkEquals(c(returns(x, t = t, period = "mtd")),
                tail(x, 1) / x[match(as.Date("2012-11-30"),t)] - 1)


    
    ## returns with weights
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



    
    
    
    ## time-weighted returns
    x <- 101:105
    checkEquals(returns(x, position = c(1, 1, 1, 1, 1)),
                returns(x))
    checkEquals(returns(x, position = c(1, 1, 1, 1, 1), pad = NA),
                returns(x, pad = NA))

    tmp <- returns(x)
    tmp[4] <- 0
    checkEquals(returns(x, position = c(1, 1, 1, 0, 0)),
                tmp)
        
    checkEquals(returns(x, position = c(1,1,2,2,3)),
                returns(x))
    checkEquals(returns(x, position = c(0,0,0,0,0)),
                rep(0, 4))
    
    pos <- c(1,1,1,2,2,0)
    price <- c(100,100,100,100,100,100)
    dim(pos) <- dim(price) <- c(3, 2)
    checkEquals(returns(price, position = pos), returns(price[ ,1]))
    checkEquals(returns(price, position = pos),
                rowSums((price*pos / rowSums(price*pos))[-3, ] * returns(price)))

    pos[ ,2] <- 0
    checkEquals(returns(price, position = pos),
                returns(price[,1]))
    
    pos1 <- c(1,1,1,2,2,2)
    pos2 <- pos1 * 2
    price <- c(101,102,103,103,105,107)
    dim(price) <- dim(pos2) <- dim(pos1) <- c(3,2)

    checkEquals(returns(price, position = pos1),
                rowSums((price*pos1 / rowSums(price*pos1))[-3, ] * returns(price)))
    checkEquals(returns(price, position = pos1),
                returns(price, position = pos2))
    

    ## from journal to time-weighted returns

    prices <- cbind(a = 101:110, b = 201:210)

    j <- journal(timestamp  = c(1,4,4,5,5,7),
                 amount     = c(1,1,1,-1,1,-1),
                 instrument = c("a", "a", "b", "a", "b", "a"),
                 price      = c(100.5,104.1,203,105,205.2,108))

    p <- position(j, when = 1:10)
    rowSums(p*prices)

}

test.scale1 <- function() {

    p <- c(104, 108, 104)
    checkEquals(scale1(p), p/104)
    checkEquals(scale1(p, when = 2), p/108)
    checkEquals(sd(returns(scale1(p, scale = TRUE))), 1)
    checkEquals(scale1(p, when = 2, scale = TRUE)[2L], 1) 

    ## NA handling
    p <- cbind(c(104, 108, 104, 105),
               c( NA, 108, 104, 105))    
    checkEquals(scale1(p), p/108)
    checkEquals(scale1(p, level = 100), p/1.08)

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

    ## TODO: currently only roughly in line because of non-constant
    ## de-meaning
    ## checkEquals(apply(returns(P), 2, sd, na.rm = TRUE),
    ##             rep(0.01, ncol(p))) ## sd is 0.01

}
