## -*- truncate-lines: t; -*-
## Time-stamp: <2014-05-08 21:02:41 CEST (es)>

test.scale0 <- function() {

    require("PMwR")
    require("RUnit")

    p <- c(104,108,104)
    PMwR:::scale0(p)

    PMwR:::scale0(p, scale = TRUE)
    
    require("zoo")
    PMwR:::scale0(zoo(p, as.Date("2010-1-1")+0:2))

    x <- structure(c(1, 1.05, 1), .Dim = c(3L, 1L))
    
    returns(x, pad = 0)   

}
