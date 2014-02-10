test.splitTrades <- function() {
    n <- c(1,1,-3,1)
    p <- c(1,2,3,2)
    tradetimes <- seq_along(n)
    splitTrades(n,p,tradetimes)

}
