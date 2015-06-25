## -*- truncate-lines: t; -*-
## Time-stamp: <2015-06-25 06:42:35 CEST (es)>

test.scale1 <- function() {

    require("PMwR")
    require("RUnit")

    p <- c(104, 108, 104)
    checkEquals(scale1(p), p/104)
    checkEquals(scale1(p, when = 2), p/108)
    checkEquals(sd(returns(scale1(p, scale = TRUE))), 1)
    checkEquals(scale1(p, when = 2, scale = TRUE)[2L], 1) 

    ## NA handling
    p <- cbind(c(104, 108, 104, 105),
               c(NA, 108, 104,105))    

    checkEquals(scale1(p), p/108)

    checkEquals(scale1(p, level = 100), p/1.08)

    ## de-mean: arithmetic
    checkEquals(apply(returns(scale1(p, centre = TRUE, geometric=FALSE)),
                      2, mean, na.rm = TRUE), 
                rep(0, ncol(p)))

    ## TODO: de-mean: geometric
    ## checkEquals(apply(returns(scale1(p, centre = TRUE, geometric=TRUE)),
    ##                   2, mean, na.rm = TRUE), 
    ##             rep(0, ncol(p)))
    
    ## scale
    checkEquals(apply(returns(scale1(p, scale = 0.01)),
                      2, sd, na.rm = TRUE),
                rep(0.01, ncol(p)))

    ## de-mean & scale 
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

}
