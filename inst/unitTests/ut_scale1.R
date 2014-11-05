## -*- truncate-lines: t; -*-
## Time-stamp: <2014-11-05 16:01:06 CET (es)>

test.scale1 <- function() {

    require("PMwR")
    require("RUnit")

    p <- c(104, 108, 104)
    checkEquals(scale1(p), p/104)
    checkEquals(scale1(p, when = 2), p/108)
    checkEquals(sd(returns(scale1(p, scale = TRUE))), 1)
    checkEquals(scale1(p, when = 2, scale = TRUE)[2L], 1) 


    p <- cbind(c(104, 108, 104, 105),
               c(NA, 108, 104,105))
    

    scale1(p, scale = 0.01)
    
}
