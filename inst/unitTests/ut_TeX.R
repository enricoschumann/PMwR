## -*- truncate-lines: t; -*-
## Time-stamp: <2014-05-26 17:58:33 CEST (es)>

test.TeXunits <- function() {

    ## TeXunits(c("1 cm", "0.7 in"), "in")
    ## TeXunits("1 cm", c("in", "cm"))

    require("PMwR")
    require("RUnit")

    tmp <- PMwR:::TeXunits(c("1 cm", "0.7 in"), c("in", "cm"))
    checkEqualsNumeric(as.numeric(tmp), 
                       c(0.393700804026445, 1.77799992492009))
    checkEquals(names(tmp) , c("in", "cm"))
}
