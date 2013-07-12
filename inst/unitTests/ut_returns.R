test.returns <- function() {

    x <- 101:112
    checkEqualsNumeric(returns(x), x[-1]/x[-length(x)]-1)
    checkEqualsNumeric(returns(x, pad = NA)[-1], x[-1]/x[-length(x)]-1)
    checkTrue(is.na(returns(x, pad = NA)[1]))


    t <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "1 day")
    x <- seq_along(t) + 100
    returns(x, t = t, period = "month")
    returns(x, t = t, period = "month", complete.first = FALSE)


    ## time-weighted returns
    x <- 101:105
    returns(x, position = c(1,1,0,0,1))
    
    
    price <- c(101,102,103,104)
    all.equal(returns(price, position = c(1,1,2,2)),
              returns(price))
    
    pos <- c(1,1,1,2,2,0)
    dim(pos) <- c(3,2)
    price <- c(100,100,100,100,100,100)
    dim(price) <- c(3, 2)
    returns(price, position = pos)
    all.equal(returns(price, position = pos), returns(price[,1]))
    
    pos1 <- c(1,1,1,2,2,2)
    pos2 <- pos1 * 2c
    dim(pos2) <- dim(pos1) <- c(3,2)
    price <- c(101,102,103,103,105,107)
    dim(price) <- c(3,2)
    all.equal(returns(price, position = pos1),
              returns(rowSums(price * pos1)))
    all.equal(returns(price, position = pos1),
              returns(price, position = pos2))
    
    
    pos <- c(1, 2, 3, 2, 3, 3)
    dim(pos) <- c(3, 2)
    price <- c(101,102,103,103,105,107)
    dim(price) <- c(3,2)
    returns(price, position = pos)
    
    
    
}
