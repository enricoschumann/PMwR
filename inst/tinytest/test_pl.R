## closed trade: vprice specified but ignored

J <- journal(amount = c(1,-1),
             price = c(1,2))

expect_equal(pl(J),
             pl(J, vprice = 3))



## --------------------------------------------------

## vprices tests
P <- structure(c(1739.49, 1752.53, 1752.79, 1745.53,
                 1734.71, 1745.72, 1773.84, 1796.94,
                 1818.51, 1800.8, 1781.6, 1769.96, 1760.69,
                 1740.48, 1751.6, 1749.51, 1739.21, 1748.72,
                 1760.33, 1760.94, 1769.21, 1790.66,
                 1784.03, 1792.28, 1786.5, 1793, 1789.21,
                 1868.77, 1869.8, 1846.89, 1847.84, 1898.01,
                 1874.97, 1902.88, 1906.86, 1891.97,
                 1901.05, 1883.16, 1891.3, 1869.44, 1862.02,
                 1877.94, 1864.72, 1892, 1887.46, 1884.58,
                 1861.64, 1828.34, 1853.25, 1858, 1870.68,
                 2008.72, 2004.2, 2049.67, 2039.87, 2050.23,
                 2079.28, 2133.91, 2150.8, 2160, 2149.87,
                 2134.87, 2155.67, 2170.22, 2153.1, 2095.97,
                 265.76, 267.1, 266.29, 263.19, 262.01,
                 261.78, 266.37, 264.29, 267.84, 267.25,
                 264.16, 259.45, 261.74, 265.58, 270.71,
                 266.92, 268.48, 270.77, 271.46, 275.15,
                 279.86, 280.41, 279.74, 280.02, 279.44,
                 284, 284.27, 289.91, 289.8, 291.52, 293.65,
                 300.35, 297.43, 299.8, 298.39, 303.19,
                 309.63, 310.33, 316.96, 312.68, 311.34,
                 315.24, 318.73, 316.57, 317.7, 319.23,
                 318.31, 308.95, 317.69, 324.34, 323.87,
                 309.51, 308.66, 318.85, 321.45, 325.21,
                 320.03, 321.55, 319.61, 327.2, 324.87,
                 324.95, 319, 323.62, 320.3, 313.05, 149.97,
                 150.34, 150.39, 149.62, 149.48, 149.59,
                 151.23, 152.03, 152.32, 151.38, 149.55,
                 149.31, 149.85, 149.93, 151.75, 151.36,
                 151.13, 151.7, 153.24, 154.53, 155.53,
                 154.69, 154.37, 155.71, 157.41, 157.41,
                 157.38, 158.67, 158.96, 157.59, 157.7,
                 160.62, 158.62, 159.03, 157.58, 160.09,
                 162.09, 161.34, 163.28, 162.13, 163.18,
                 166.17, 167.1, 166.5, 165.7, 166.72,
                 165.04, 162.28, 165.46, 168.04, 172.78,
                 170.23, 174.38, 180.12, 179.9, 183.63,
                 183.89, 188.7, 184.44, 184.71, 183.71,
                 185.35, 187.23, 187.28, 184.42, 178.59),
               .Dim = c(66L, 3L),
               .Dimnames = list(
                   NULL, c("AMZN", "AAPL", "MSFT")))

timestamp.P <- structure(
    c(18215, 18218, 18219, 18220,
      18221, 18222, 18225, 18226,
      18227, 18229, 18232, 18233,
      18234, 18235, 18236, 18239,
      18240, 18241, 18242, 18243,
      18246, 18247, 18248, 18249,
      18250, 18253, 18254, 18256,
      18257, 18260, 18261, 18263,
      18264, 18267, 18268, 18269,
      18270, 18271, 18274, 18275,
      18276, 18277, 18278, 18282,
      18283, 18284, 18285, 18288,
      18289, 18290, 18291, 18292,
      18295, 18296, 18297, 18298,
      18299, 18302, 18303, 18304,
      18305, 18306, 18310, 18311,
      18312, 18313), class = "Date")

library("PMwR")
J <- journal()
for (asset in colnames(P)) {

    amount <- sample(c(-20, -10, 10, 20), size = 5, replace = TRUE)
    timestamp <- sample(timestamp.P, size = 5, replace = TRUE)
    J <- c(J,
           journal(amount = amount,
                   timestamp = timestamp,
                   price = P[match(timestamp, timestamp.P), asset],
                   instrument = asset))
}


J <- structure(list(timestamp = structure(
                        c(18257, 18302, 18291, 18260, 18276, 18271,
                          18284, 18232, 18246, 18304, 18227, 18254,
                          18312, 18232, 18267), class = "Date"),
                    amount = c(10, -10, -10, -10, 10, 10, 20,
                               10, -10, -10, 20, -20, 10, -10, 10),
                    price = c(1869.8, 2133.91, 1870.68, 1846.89,
                              1862.02, 310.33, 319.23, 264.16,
                              279.86, 327.2, 152.32, 157.38, 184.42,
                              149.55, 159.03),
                    instrument = c("AMZN", "AMZN", "AMZN", "AMZN",
                                   "AMZN", "AAPL", "AAPL", "AAPL",
                                   "AAPL", "AAPL", "MSFT", "MSFT",
                                   "MSFT", "MSFT", "MSFT")),
               class = "journal")


## J <- structure(list(timestamp = structure(c(18218, 18292, 18218, 18242,
##                                             18312, 18284, 18233, 18246, 18233, 18257, 18313, 18229, 18246,
##                                             18220, 18226, 18270, 18303, 18232, 18275, 18306, 18219, 18235,
##                                             18229, 18302, 18298, 18236, 18285, 18256, 18226, 18263), class = "Date"),
##                     amount = c(-10, 20, 10, 20, 20, 20, -10, -10, 20, -10, 20,
##                                20, -20, -20, -10, 20, 20, -10, 20, -10, -10, -20, -20, -20,
##                                20, -20, -10, 20, 10, -20), price = c(1752.53, 2008.72, 1752.53,
##                                                                      1760.33, 2153.1, 1884.58, 1769.96, 1769.21, 1769.96, 1869.8,
##                                                                      313.05, 267.25, 279.86, 263.19, 264.29, 309.63, 319.61, 264.16,
##                                                                      312.68, 324.95, 150.39, 149.93, 151.38, 188.7, 183.63, 151.75,
##                                                                      165.04, 158.67, 152.03, 160.62), instrument = c("AMZN", "AMZN",
##                                                                                                                      "AMZN", "AMZN", "AMZN", "AMZN", "AMZN", "AMZN", "AMZN", "AMZN",
##                                                                                                                      "AAPL", "AAPL", "AAPL", "AAPL", "AAPL", "AAPL", "AAPL", "AAPL",

## | timestamp        | amount |   price | instrument |
## |------------------+--------+---------+------------|
## | [2019-12-27 Fri] |     10 |  1869.8 | AMZN       |
## | [2020-02-10 Mon] |    -10 | 2133.91 | AMZN       |
## | [2020-01-30 Thu] |    -10 | 1870.68 | AMZN       |
## | [2019-12-30 Mon] |    -10 | 1846.89 | AMZN       |
## | [2020-01-15 Wed] |     10 | 1862.02 | AMZN       |
## | [2020-01-10 Fri] |     10 |  310.33 | AAPL       |
## | [2020-01-23 Thu] |     20 |  319.23 | AAPL       |
## | [2019-12-02 Mon] |     10 |  264.16 | AAPL       |
## | [2019-12-16 Mon] |    -10 |  279.86 | AAPL       |
## | [2020-02-12 Wed] |    -10 |   327.2 | AAPL       |
## | [2019-11-27 Wed] |     20 |  152.32 | MSFT       |
## | [2019-12-24 Tue] |    -20 |  157.38 | MSFT       |
## | [2020-02-20 Thu] |     10 |  184.42 | MSFT       |
## | [2019-12-02 Mon] |    -10 |  149.55 | MSFT       |
## | [2020-01-06 Mon] |     10 |  159.03 | MSFT       |

J
##     instrument   timestamp  amount    price
## 1         AMZN  2019-12-27      10  1869.80
## 2         AMZN  2020-02-10     -10  2133.91
## 3         AMZN  2020-01-30     -10  1870.68
## 4         AMZN  2019-12-30     -10  1846.89
## 5         AMZN  2020-01-15      10  1862.02
## 6         AAPL  2020-01-10      10   310.33
## 7         AAPL  2020-01-23      20   319.23
## 8         AAPL  2019-12-02      10   264.16
## 9         AAPL  2019-12-16     -10   279.86
## 10        AAPL  2020-02-12     -10   327.20
## 11        MSFT  2019-11-27      20   152.32
## 12        MSFT  2019-12-24     -20   157.38
## 13        MSFT  2020-02-20      10   184.42
## 14        MSFT  2019-12-02     -10   149.55
## 15        MSFT  2020-01-06      10   159.03
##
## 15 transactions


pl(J)
## AAPL
##   P/L total           NA
##   average buy   303.2375
##   average sell    303.53
##   cum. volume         60
##
## AMZN
##   P/L total           NA
##   average buy    1865.91
##   average sell  1950.493
##   cum. volume         50
##
## MSFT
##   P/L total           NA
##   average buy   162.0225
##   average sell    154.77
##   cum. volume         70
##
## ‘P/L total’ is in units of instrument;
## ‘volume’ is sum of /absolute/ amounts.
## ‘sum(amount)’ is not zero for AAPL: specify ‘vprice’ to compute P/L.
## ‘sum(amount)’ is not zero for AMZN: specify ‘vprice’ to compute P/L.
## ‘sum(amount)’ is not zero for MSFT: specify ‘vprice’ to compute P/L.

position(J)
##      2020-02-20
## AAPL         20
## AMZN        -10
## MSFT         10


pl(J, vprice = c(AMZN = 2020, MSFT = 178, AAPL = 311))


str(timestamp.P)
## Date[1:66], format: "2019-11-15" "2019-11-18" ...

str(P)
## num [1:66, 1:3] 1739 1753 1753 1746 1735 ...
## - attr(*, "dimnames")=List of 2
##  ..$ : NULL
##  ..$ : chr [1:3] "AMZN" "AAPL" "MSFT"


PL <- pl(J, vprice = P, along.timestamp = timestamp.P)
PL <- sapply(PL, `[[`, "pl")
tail(PL, 5)
##             AAPL   AMZN  MSFT
## 2020-02-14 440.1 -152.1   6.4
## 2020-02-18 321.1 -360.1   6.4
## 2020-02-19 413.5 -505.6   6.4
## 2020-02-20 347.1 -334.4   6.4
## 2020-02-21 202.1  236.9 -51.9

library("datetimeutils")
ii <- c(1, nth_day(timestamp.P,  ## extract position of last day of month
                   period = "month", n = "last",
                   index = TRUE))
diff(PL[ii, ])
##              AAPL   AMZN  MSFT
## 2019-11-29    0.0    0.0 -18.8
## 2019-12-31  157.0 -229.1  38.5
## 2020-01-31 -202.6   86.6 -13.3
## 2020-02-21  247.7  379.4 -58.3




ii <- J$instrument == "AMZN"
J$price[ii] <- P[match(J["AMZN"]$timestamp, timestamp.P), "AMZN"]



##

expect_equal(pl(amount = c(1,-1),
               price  = c(1,2))[[1]][["pl"]], 1)

expect_equal(pl(amount = c(1,-1), price = c(1,2)),
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

ans <- pl(amount = 1, price = 1,
          initial.position = 1, initial.price = 1,
          vprice = 2)
attr(ans, "footnotes") <- NULL
expect_equal(ans,
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

expect_equal(x,
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

## pl(pl(...))
expect_equal(pl(x),
            structure(c(2, 60), .Names = c("Equity A", "Equity B")))

expect_equal(as.data.frame(x),
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
expect_true(is.na(suppressWarnings(pl(jnl)[[1]][["pl"]])))



## initial position
tmp <- pl(journal(),
          initial.position = 1,
          initial.price = 100,
          vprice = 105)
expect_equal(tmp[[1]][["pl"]], 5)

tmp <- pl(journal(),
          initial.position = c(A = 1, B = 2),
          initial.price = c(A = 100),
          vprice = c(A = 105, B = 110))
expect_equal(tmp[[1]][["pl"]],5)
expect_true(is.na(tmp[[2]][["pl"]]))

tmp <- pl(journal(),
          initial.position = c(A = 1, B = 2),
          initial.price = c(A = 100, B = 100),
          vprice = c(A = 105, B = 110))
expect_equal(unlist(lapply(tmp, `[[`, "pl")), c(A=5,B=20))

tmp <- pl(journal(),
          initial.position = c(A = 1, B = 2),
          initial.price = c(A = 100, B = 100),
          vprice = c(B = 110, A = 105))
expect_equal(unlist(lapply(tmp, `[[`, "pl")), c(A=5,B=20))

tmp <- pl(journal(), multiplier = 2,
          initial.position = c(A = 1, B = 2),
          initial.price = c(A = 100, B = 100),
          vprice = c(A = 105, B = 110))
expect_equal(unlist(lapply(tmp, `[[`, "pl")), c(A=10,B=40))


## initial position can be a 'position'
tmp <- pl(journal(),
          initial.position = position(amount = c(1,2),
                                      instrument = c("A", "B")),
          initial.price = c(A = 100, B = 100),
          vprice = c(A = 105, B = 110))
expect_equal(unlist(lapply(tmp, `[[`, "pl")), c(A=5,B=20))

tmp <- pl(journal(),
          initial.position = position(amount = c(2,1),
                                      instrument = c("B", "A")),
          initial.price = c(A = 100, B = 100),
          vprice = c(A = 105, B = 110))
expect_equal(unlist(lapply(tmp, `[[`, "pl")), c(A=5,B=20))


## initial position can be a 'journal'
tmp <- pl(journal(),
          multiplier = c(A = 2,
                         B = 4),
          initial.position = journal(amount     = c(2,1),
                                     instrument = c("B", "A")),
          initial.price = c(A = 100,
                            B = 100),
          vprice = c(A = 105, B = 110))
expect_equal(unlist(lapply(tmp, `[[`, "pl")), c(A=10,B=80))


## amount <- c(1,1,-1,1,-1)
## price <- c(100,99,101,100,101)
## pl(amount, price, along.timestamp = TRUE)

## amount <- c(1,-2,1)
## price <- c(100,101,100)
## pl(amount, price)
## pl(amount, price, along.timestamp = TRUE)

## tmp <- splitTrades(amount, price, timestamp = seq_along(amount),
##                    aggregate = TRUE)


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


## library("PMwR")
## library("RUnit")

## multiplier
expect_equal(pl(amount = c(1, -1),
               price  = c(1,  2),
               multiplier = 0)[[1L]][["pl"]], 0)
expect_equal(pl(amount = c(1, -1),
               price  = c(1,  2),
               multiplier = 1)[[1L]][["pl"]], 1)
expect_equal(pl(amount = c(1, -1),
               price  = c(1,  2))[[1L]][["pl"]], 1)
expect_equal(pl(amount = c(1, -1),
               price  = c(1,  2),
               multiplier = 2)[[1]][["pl"]], 2)

expect_equal(pl(amount = c(1, -1),
               price  = c(1,  2),
               instrument = c("B", "B"),
               multiplier = c(A = 1, B = 2))[[1L]][["pl"]], 2)



## along.timestamp: user-specified: open trade
j <- journal(amount = 1,
             timestamp = 2.5,
             price = 102)
res <- pl(j, along.timestamp = 1:10,
          vprice = 101:110)
expect_equal(res[[1]]$timestamp, 1:10)

### .... total pnl
expect_equivalent(res[[1]]$pl,
                  c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8))

### .... and split pnl
expect_equivalent(res[[1]]$pl[-(1:2)], 1:8)
expect_equivalent(res[[1]]$unrealised[-(1:2)], 1:8)
expect_true(all(res[[1]]$realised[-(1:2)] == 0))

### .... pnl before trades should be either 0 or NA
expect_true(all(is.na(res[[1]]$pl[(1:2)]) | res[[1]]$pl[(1:2)]==0L))
expect_true(all(is.na(res[[1]]$realised) | res[[1]]$realised==0))




## along.timestamp: user-specified: closed trade
j <- journal(amount = c(1,-1),
             timestamp = c(2.5,9),
             price = c(102, 109))
res <- pl(j, along.timestamp = 1:10,
          vprice = 101:110)
expect_equal(res[[1]]$timestamp, 1:10)

### .... total pnl
expect_equivalent(res[[1]]$pl,
                   c(0, 0, 1, 2, 3, 4, 5, 6, 7, 7))

### .... and split pnl
expect_equivalent(res[[1]]$pl[-(1:2)], c(1:7,7))
expect_equivalent(res[[1]]$unrealised[-(1:2)],
                   c(1, 2, 3, 4, 5, 6, 0, 0))
expect_equivalent(res[[1]]$realised[-(1:2)],
                   c(0, 0, 0, 0, 0, 0, 7, 7))

### .... pnl before trades should be either 0 or NA
expect_true(all(is.na(res[[1]]$pl[(1:2)]) | res[[1]]$pl[(1:2)]==0L))




## along.timestamp
j <- journal(amount = c(1,-1),
             timestamp = c(1,2.5),
             price = c(100,101))
pl(j, along.timestamp = TRUE)
expect_equivalent(pl(j, along.timestamp = TRUE)[[1]]$pl,
                   0:1)

### ... journal timestamp
expect_equal(pl(j, along.timestamp = TRUE)[[1]]$timestamp,
            c(1,2.5))

tmp <- pl(amount = 1, timestamp = 0, price = 100,
          vprice = 101:110, along.timestamp = 1:10)
expect_equivalent(tmp[[1]]$pl, 1:10)
### ... custom timestamp
expect_equal(tmp[[1]]$timestamp, 1:10)

## should work since vprice is timestamp-agnostic:
## it just computes the current PL, no matter the
## time
res <- pl(journal(),
          initial.position = 1, initial.price = 100,
          vprice = 101)
expect_equal(pl(res), 1)
expect_equal(res[[1]]$buy, 100)
expect_equal(res[[1]]$sell, 101)
expect_equal(res[[1]]$volume, 0)


## should *not* work since the initial price has no
## timestamp, but for vprice the timestamps are
## specified
expect_error(
    pl(journal(),
       initial.position = 1, initial.price = 1,
       vprice = 101:110, along.timestamp = 1:10))


res <- pl(journal(amount = 1,
                  price = 2.5,
                  timestamp = 3),
          along.timestamp = 1:10,
          vprice = 1:10)
expect_equal(res[[1]]$timestamp, 1:10)
expect_equivalent(res[[1]]$unrealised[10], 7.5)
expect_equivalent(res[[1]]$realised[10],   0.0)

res <- pl(journal(amount = 1:2,
                  price = 1:2,
                  timestamp = 3),
          along.timestamp = 1:10,
          vprice = 1:10)

expect_equal(res[[1]]$volume,
            c(0, 0, 3, 3, 3, 3, 3, 3, 3, 3))
expect_equivalent(res[[1]]$unrealised[3], 4)
expect_equivalent(res[[1]]$unrealised[4], 7)




## unsorted timestamp: gets sorted
jnl <- journal(price  = c( 90, 50, 100),
               amount = c(  1,  1,  -2),
               timestamp = 3:1)
expect_equal(pl(jnl, along.timestamp = TRUE)[[1]]$timestamp, 1:3)

## implicit timestamp
jnl <- journal(price  = c( 90, 50, 100),
               amount = c(  1,  1,  -2))
expect_equal(pl(jnl, along.timestamp = TRUE)[[1]]$timestamp, 1:3)



## data.frame method
D <- data.frame(price = c(100,102),
                amount = c(1,-1))
J <- journal(price = c(100,102),
             amount = c(1,-1))
expect_equal(pl(D), pl(J))


## empty journal: pl/volume should be zero
expect_equal(pl(journal()),
            structure(list(
                structure(list(pl = 0,
                               realised = NA,
                               unrealised = NA,
                               buy = NaN,
                               sell = NaN,
                               volume = 0),
                          .Names = c("pl", "realised", "unrealised",
                                     "buy", "sell", "volume"))),
                class = "pl",
                along.timestamp = FALSE,
                instrument = NA))




## https://quant.stackexchange.com/questions/36505/calculate-day-to-day-change-in-value-of-open-position
j <- journal(amount = c(100, -100),
             price = c(7418.04, 7433.29),
             timestamp = as.POSIXct(c("20070829  1400",
                                      "20070829  1724"),
                                    format = "%Y%m%d  %H%M"))
expect_equal(pl(j)[[1]]$pl, 1525)
expect_equal(pl(j),
            structure(list(structure(
                list(pl = 1525,
                     realised = NA,
                     unrealised = NA,
                     buy = 7418.04,
                     sell = 7433.29,
                     volume = 200),
                .Names = c("pl", "realised",
                           "unrealised",
                           "buy", "sell",
                           "volume"))),
                class = "pl",
                along.timestamp = FALSE,
                instrument = NA))

timestamp <- as.POSIXct(
    c("20070829  0900", "20070829  1000", "20070829  1100",
      "20070829  1200", "20070829  1300", "20070829  1400",
      "20070829  1500", "20070829  1600", "20070829  1724",
      "20070830  0900", "20070830  1000", "20070830  1100",
      "20070830  1200", "20070830  1300", "20070830  1400",
      "20070830  1500", "20070830  1600", "20070830  1724",
      "20070831  0900", "20070831  1000"),
    format = "%Y%m%d  %H%M")

close <- c(7372.1 , 7372.16, 7428.72, 7418.13, 7422.03,
           7418.04, 7426.8 , 7414.65, 7433.29, 7478.72,
           7464.2 , 7475.08, 7456.95, 7429.93, 7444.99,
           7420.8 , 7479.4 , 7487.42, 7554.82, 7552.3)

expect_equal(pl(j, along.timestamp = timestamp, vprice = close)[[1]]$pl,
            structure(c(0, 0, 0, 0, 0, 0, 876, -339, 1525, 1525, 1525, 1525,
                        1525, 1525, 1525, 1525, 1525, 1525, 1525, 1525),
                      .Names = c("2007-08-29 09:00:00",
                                 "2007-08-29 10:00:00",
                                 "2007-08-29 11:00:00",
                                 "2007-08-29 12:00:00",
                                 "2007-08-29 13:00:00",
                                 "2007-08-29 14:00:00",
                                 "2007-08-29 15:00:00",
                                 "2007-08-29 16:00:00",
                                 "2007-08-29 17:24:00",
                                 "2007-08-30 09:00:00",
                                 "2007-08-30 10:00:00",
                                 "2007-08-30 11:00:00",
                                 "2007-08-30 12:00:00",
                                 "2007-08-30 13:00:00",
                                 "2007-08-30 14:00:00",
                                 "2007-08-30 15:00:00",
                                 "2007-08-30 16:00:00",
                                 "2007-08-30 17:24:00",
                                 "2007-08-31 09:00:00",
                                 "2007-08-31 10:00:00")))
