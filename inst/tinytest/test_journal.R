## a simple journal
j <- journal(amount = 1:2)

## length
expect_equal(length(j), 2)

## 'account' and 'id' missing: NULL
expect_true(is.null(j$account))
expect_true(is.null(j$id))

## 'timestamp', 'instrument' and 'price' missing: NA
expect_true(all(is.na(j$timestamp)))
expect_true(all(is.na(j$instrument)))
expect_true(all(is.na(j$price)))

## ... but length is same as length(amount)
expect_equal(length(j$timestamp),  length(j))
expect_equal(length(j$instrument), length(j))
expect_equal(length(j$price),      length(j))

## empty journals
expect_equal(journal(),
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
## expect_equal(as.journal(data.frame(amount = numeric(0),
##                                   comment = character(0),
##                                   stringsAsFactors = FALSE)),
##             structure(list(amount = numeric(0),
##                            comment = character(0)),
##                       .Names = c("amount", "comment"),
##                       class = "journal"))

expect_equal(journal(),
             c(journal(), journal()))
expect_equal(journal(),
             c(journal(), journal(), journal()))
expect_equal(summary(journal()),
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
expect_equal(c(j, journal()) , j)
## expect_equal(c(journal(), j), j)   ## TODO: sorting of fields

## subsetting
## j[1]
expect_equal(j["stock"], j)
expect_equal(length(j["bla"]), 0)

## method: c
jj <- c(j, j)

## method: length
expect_equal(length(jj), 10L)

## method: sort
expect_equal(sort(jj)$timestamp, rep(1:5, each = 2))
expect_equal(sort(jj, decreasing = TRUE)$timestamp, rep(5:1, each = 2))


j <- journal(amount = 1:4,
             instrument = c("a", "b", "a", "b"),
             timestamp = c(1,1,2,2))
expect_equal(sort(j)$timestamp, c(1,1,2,2))
expect_equal(sort(j, decreasing = TRUE)$timestamp, c(2,2,1,1))

sj <- sort(j, by = c("instrument", "timestamp"))
expect_equal(sj$timestamp, c(1,2,1,2))
expect_equal(sj$instrument, c("a", "a", "b", "b"))
expect_equal(sj$amount, c(1,3,2,4))

sj <- sort(j, by = c("instrument", "timestamp"),
           decreasing = TRUE)
expect_equal(sj$timestamp, rev(c(1,2,1,2)))
expect_equal(sj$instrument, rev(c("a", "a", "b", "b")))
expect_equal(sj$amount, rev(c(1,3,2,4)))


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
expect_equal(journal(amount = 1, foo = 1:10),
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
expect_equal(journal(amount = 1, foo = 1:10),
             journal(amount = rep(1, 10), foo = 1:10))

expect_equal(journal(amount = 1:10, foo = 1),
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
expect_equal(journal(amount = 1:10, foo = 1),
             journal(amount = 1:10, foo = rep(1, 10)))


## assignment
j <- journal(amount=1:3)

## ok: replace field
j$amount[1] <- 5   ## ok
expect_equal(j$amount, c(5,2,3))

## not ok: replace journal as a whole
expect_error(j[1]$amount <- 10)

