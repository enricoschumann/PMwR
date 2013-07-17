test.pl <- function() {

    require("PMwR")
    require("RUnit")
    
    n <- c(1,1,-3,-1,2)
    p <- 100 + 1:length(n)

    pl(amount = c(1,-1),
       price  = c(1,2))
}
