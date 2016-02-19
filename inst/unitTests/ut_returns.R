test.returns <- function() {

    require("PMwR", quietly = TRUE)
    require("RUnit", quietly = TRUE)
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
    ## (when there is no period, methods are required to keep
    ## timestamp information for themselves and then to re-assemble
    ## the necessary class structure)
    x <- 101:112
    t <- seq_along(x)
    suppressWarnings(checkEquals(returns(x, period = "month"), returns(x)))
    suppressWarnings(checkEquals(returns(x, t = t),            returns(x)))
    
    ## period -- check class
    require("PMwR", quietly = TRUE)
    require("RUnit", quietly = TRUE)
    require("zoo", quietly = TRUE, warn.conflicts = FALSE)
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


    ## mtab (internal)
    ## require("database")
    ## tmp <- fetchTable("daily2", "lu0891409947")
    ## x <- returns(tmp$close, tmp$timestamp, period = "monthly")

    x <- structure(c(-0.0163, 0.00274473924977126,
                     -0.0096309813463098, 0.0110553792609274, 0.010023286423003,
                     -0.00661587810745801, -0.00625630676084754, 0.00781884646628761,
                     0.0050377833753148, 0.00511278195488729, 0.0072810692200278,
                     0.0108921675413407, 0.00528945048486618, -9.74373964727215e-05,
                     0.00993958292730457, -0.00984175993824776, 0.00613915416098232,
                     0.0030992736077482, -0.00337935695664759, 0.0117225343925598,
                     -0.00641578090587003, -0.0161912104857364, -0.00186128526645768,
                     -0.00853862008047901, -0.0180162344090279),
                   t = structure(c(15884L,
                   15917L, 15947L, 15978L, 16009L, 16038L, 16069L, 16101L, 16129L,
                   16160L, 16190L, 16220L, 16251L, 16282L, 16311L, 16343L, 16374L,
                   16402L, 16434L, 16465L, 16493L, 16525L, 16555L, 16584L, 16610L), class
                   = "Date"), period = "monthly", class = "p_returns")
    
    
    .mtab(x)
    .mtab(x, plus = TRUE)
    .mtab(x, zero.print = "_____")
    .mtab(x, zero.print = "_____", plus = TRUE)
    .mtab(x, month.names = LETTERS[1:12], ytd = "Z")
    .mtab(x, digits = 0)
    .mtab(x, digits = 0, zero.print = "___")


    ## rebalanced returns
    
}
