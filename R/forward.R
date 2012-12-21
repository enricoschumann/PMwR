forward <- function(spot, t2m, r, q = NULL, cf = NULL, t2cf = NULL) {
    if (!is.null(q) && !is.null(cf))
        stop("specify either 'q' or 'cf'")
    if (t2m < 0)
        return(NA)
    if (is.null(cf) && is.null(q))
        q <- 0
    if (!is.null(q)) {
        f <- spot * exp((r - q) * t2m)
    } else {
        f <- spot -  sum(exp(-r * t2cf) * cf)
    }
    f
}

#f <- forward(spot=100, t2m = 1, r = 0.1, cf = c(1,2), t2cf = c(.3,.6))
#f <- forward(spot=100, t2m = 1, r = 0.1)
#f <- forward(spot=100, t2m = 1, r = 0.1, q= 0.02)



spot <- function(forward, t2m, r, q = NULL, cf = NULL, t2cf = NULL) {
    if (!is.null(q) && !is.null(cf))
        stop("specify either 'q' or 'cf'")
    if (t2m < 0)
        return(NA)
    if (is.null(cf) && is.null(q))
        q <- 0
    if (!is.null(q)) {
        s <- forward * exp((q - r) * t2m)
    } else {
        s <- forward +  sum(exp(-r * t2cf) * cf)
    }
    s
}

#s <- spot(f, t2m = 1, r = 0.1, cf = c(1,2), t2cf = c(.3,.6))
#s <- spot(f, t2m = 1, r = 0.1)
#s <- spot(f, t2m = 1, r = 0.1, q= 0.02)


putCallParity <- function(call = NULL, put = NULL, forward = NULL,
    strike, t2m, r, q = NULL, cf = NULL, t2cf = NULL) {
    if(is.null(call) + is.null(put) + is.null(forward) != 1L)
        stop("provide call/put, call/forward, or put/forward")
    if(is.null(call))
        res <- put  + exp(-r * t2m) * (forward - strike)
    if(is.null(put))
        res <- call - exp(-r * t2m) * (forward - strike)
    if(is.null(forward))
        res <- (call - put) * exp(r*t2m) + strike
    res
}
