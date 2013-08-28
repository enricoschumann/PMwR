## -*- truncate-lines: t; -*-

test.journal <- function() {

    require("PMwR")
    require("RUnit")
    
    ## the simplest journal
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
    

    ## empty journal
    checkEquals(journal(),
                structure(list(timestamp = numeric(0),
                               amount = numeric(0),
                               price = numeric(0), 
                               instrument = character(0)),
                          .Names = c("timestamp", "amount", 
                                     "price", "instrument"),
                          class = "journal"))


    ## a more reasonable journal
    timestamp <- 1:5
    amount <- 1
    price <- 101:105
    instrument <- "Stock A"
    j <- journal(timestamp, amount, price, instrument = instrument)

    ## method: c
    checkEquals(c(j, journal()) , j)    

    ## subsetting
    j[1]
    checkEquals(j["stock"], j)
    checkEquals(x <- j["bla"], journal())
    
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
    j <- journal(timestamp, amount, price, instrument = instrument)
    head(j, 4)
    head(j, 4, by = FALSE)

    


}
