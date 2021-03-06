expect_true(all(pricetable(1:10) == 1:10))

expect_equivalent(as.matrix(pricetable(1:10, timestamp = 1:10, instrument = "A")),
                  pricetable(1:10, timestamp = 1:10, instrument = "A")[, "A"])

expect_equivalent(as.matrix(pricetable(1:10, timestamp = 1:10, instrument = "A")),
                  pricetable(1:10, timestamp = 1:10, instrument = "A")[1:10, "A"])

expect_equivalent(as.matrix(pricetable(1:10, timestamp = 1:10, instrument = "A")),
                  pricetable(1:10, timestamp = 1:10, instrument = "A")[1:10,])


## repeated column
pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[, c("A","A"), as.matrix=FALSE]
expect_equal(attr(pt, "instrument"), c("A","A"))

pt <- pricetable(1:3, timestamp = 1:3, instrument = "A")[, c("A","A")]
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
