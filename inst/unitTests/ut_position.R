test.position <- function() {

    ## require("PMwR")
    ## require("RUnit")

    
    checkEqualsNumeric(position(amount = 1:5), 15)

    ## at least 'amount' needs to be specified
    checkException(position(), silent = TRUE)

    
    ## construct from raw data or from journal
    t <- 1:5
    n <- c(1, 1, -1, 3, -4)
    j <- journal(timestamp = t, amount = n)

    position(amount = n, timestamp = t, when = 4.9)    

    
    position(amount = n, timestamp = t, when = c(-10,1.4,4.9))

    position(j, when = 4.9)
    position(j, when = c(-10,1.4,4.9))


}
