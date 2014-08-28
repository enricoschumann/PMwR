## -*- truncate-lines: t; -*-
## Time-stamp: <2014-08-08 11:56:13 CEST (es)>

test.expstr <- function() {

    require("PMwR")
    require("RUnit")

    s <- c("", "a  s")
    s1 <- expstr(s, after = "^", width = 20)
    checkEquals(nchar(s1), c(20L, 20L))

    s2 <- expstr(s, at = 0, width = 20)
    checkEquals(nchar(s2), c(20L, 20L))
    
}
