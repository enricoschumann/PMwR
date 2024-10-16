expect_true(all(pricetable(1:10) == 1:10))

expect_equivalent(
    as.matrix(pricetable(1:10, timestamp = 1:10, instrument = "A")),
    pricetable(1:10, timestamp = 1:10, instrument = "A")[, "A"])

expect_equivalent(
    as.matrix(pricetable(1:10, timestamp = 1:10, instrument = "A")),
    pricetable(1:10, timestamp = 1:10, instrument = "A")[1:10, "A"])

expect_equivalent(
    as.matrix(pricetable(1:10, timestamp = 1:10, instrument = "A")),
    pricetable(1:10, timestamp = 1:10, instrument = "A")[1:10,])


## repeated column
pt <- pricetable(1:3,
                 timestamp = 1:3,
                 instrument = "A")[, c("A","A"), as.matrix = FALSE]
expect_equal(attr(pt, "instrument"), c("A","A"))

pt <- pricetable(1:3,
                 timestamp = 1:3, instrument = "A")[, c("A","A")]
expect_equal(colnames(pt), c("A","A"))

expect_equivalent(as.matrix(pt),
                  structure(c(1L, 2L, 3L, 1L, 2L, 3L),
                            .Dim = c(3L, 2L),
                            timestamp = 1:3,
                            instrument = c("A", "A")))

## repeated column + NA column
pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[ ,c("A","B","A")]
expect_equal(colnames(pt), c("A", "B", "A"))
expect_equivalent(as.matrix(pt),
                  structure(c(1L, 2L, 3L, NA, NA, NA, 1L, 2L, 3L),
                            .Dim = c(3L, 3L),
                            timestamp = 1:3,
                            instrument = c("A", "B", "A")))


## repeated column + NA column + NA rows
pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[0:4 ,c("A","B","A")]
expect_equivalent(as.matrix(pt),
                  structure(c(NA, 1L, 2L, 3L, NA,
                              NA, NA, NA, NA, NA,
                              NA, 1L, 2L, 3L, NA),
                            .Dim = c(5L, 3L),
                            timestamp = 0:4,
                            instrument = c("A", "B", "A")))

pt1 <- pricetable(1:3, timestamp = 1:3, instrument = "A")[0:4 ,c("A","B","A")]
pt2 <- pricetable(1:3, timestamp = 1:3, instrument = "A")[0:4 ,c("A","B","A"), missing = -99]
expect_true(all(unclass(pt2)[is.na(pt1), drop = FALSE] == -99))


pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[0:4 ,c("A","B","A"), missing = "locf"]
expect_equivalent(as.matrix(pt),
                  structure(c(NA, 1L, 2L, 3L, 3L,
                              NA, NA, NA, NA, NA,
                              NA, 1L, 2L, 3L, 3L),
                            .Dim = c(5L, 3L),
                            timestamp = 0:4,
                            instrument = c("A", "B", "A")))


## setting up a pt: vector => matrix
expect_true(all(dim(pricetable(1:2, instrument = c("A", "B"))) == c(1, 2)))
expect_true(all(dim(pricetable(1:2, timestamp = 1:2)) == c(2,1)))
expect_equal(pricetable(1:2, instrument = c("A", "B")),
             pricetable(c(A = 1, B = 2)))



## extract, but keep pricetable class
pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[, "A", as.matrix = FALSE]
expect_equal(pt, pricetable(1:3, timestamp = 1:3, instrument = "A"))

pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[1:3, , as.matrix = FALSE]
expect_equal(pt, pricetable(1:3, timestamp = 1:3, instrument = "A"))




## ------------------

P <- pricetable(cbind(11:13,
                      21:23),
                timestamp = 1:3,
                instrument = c("A", "B"))
ans <- as.matrix(P[1:5, c("C", "D")])
##    C  D
## 1 NA NA
## 2 NA NA
## 3 NA NA
## 4 NA NA
## 5 NA NA

t <- structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
               dim = c(5L, 2L),
               dimnames = list(c("1", "2", "3", "4", "5"),
                               c("C", "D")))
expect_equivalent(t, ans)


## ------------------

P <- pricetable(cbind(11:13,
                      21:23),
                timestamp = as.Date("2010-4-1") + 0:2,
                instrument = c("A", "B"))
ans <- P[as.Date("2010-04-01")+1:10, c("C", "D")]
##             C  D
## 2010-04-02 NA NA
## 2010-04-03 NA NA
## 2010-04-04 NA NA
## 2010-04-05 NA NA
## 2010-04-06 NA NA
## 2010-04-07 NA NA
## 2010-04-08 NA NA
## 2010-04-09 NA NA
## 2010-04-10 NA NA
## 2010-04-11 NA NA

t <- structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA),
               dim = c(10L, 2L),
               dimnames = list(c("2010-04-02", "2010-04-03",
                                 "2010-04-04", "2010-04-05",
                                 "2010-04-06", "2010-04-07",
                                 "2010-04-08", "2010-04-09",
                                 "2010-04-10", "2010-04-11"),
                               c("C", "D")))
expect_equivalent(t, ans)


## ------------------

ans <- P[as.Date("2010-04-01") + 1:10, c("C", "D", "A")]
##             C  D  A
## 2010-04-02 NA NA 12
## 2010-04-03 NA NA 13
## 2010-04-04 NA NA NA
## 2010-04-05 NA NA NA
## 2010-04-06 NA NA NA
## 2010-04-07 NA NA NA
## 2010-04-08 NA NA NA
## 2010-04-09 NA NA NA
## 2010-04-10 NA NA NA
## 2010-04-11 NA NA NA

t <- structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA, 12L, 13L, NA, NA, NA, NA, NA,
                 NA, NA, NA), dim = c(10L, 3L),
               dimnames = list(c("2010-04-02",
                                 "2010-04-03",
                                 "2010-04-04",
                                 "2010-04-05",
                                 "2010-04-06",
                                 "2010-04-07",
                                 "2010-04-08",
                                 "2010-04-09",
                                 "2010-04-10",
                                 "2010-04-11"), c("C", "D", "A")))
expect_equivalent(t, ans)


## ------------------

ans <- P[as.Date("2010-04-05"), c("C", "D", "A")]
t <- structure(c(NA, NA, NA),
               dim = c(1L, 3L),
               dimnames = list("2010-04-05", c("C", "D", "A")))
expect_equivalent(t, ans)

ans <- P[as.Date("2010-04-05"), c("C", "D", "A"), missing = "locf"]
t <- structure(c(NA, NA, 13),
               dim = c(1L, 3L),
               dimnames = list("2010-04-05", c("C", "D", "A")))
expect_equivalent(t, ans)


