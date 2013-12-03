test.runStats <- function() {

    loopFun <- function(what, y, N) {
        f <- get(what)  ## works only if what is function name!
        tmp <- numeric(length(y))
        for (i in N:length(y))
            tmp[i] <- f(y[(i-N+1):i])
        tmp
    }
    whats <- c("min", "max", "mean", "sum")
    for (what in whats) {
        y <- rnorm(100); N <- 5
        checkEquals(loopFun(what,y, N), runStats(what, y, N))
        y <- rnorm(100); N <- 1;
        checkEquals(loopFun(what,y, N), runStats(what, y, N))
        ## with N == 1, function should evaluate to y
        checkEquals(y, runStats(what, y, N))
        y <- rep.int(1,100); N <- 1;
        checkEquals(loopFun(what,y, N), runStats(what, y, N))
    }

    ## exception: N < 1
    y <- rnorm(100); N <- 0
    checkException(runStats(what, y, N), silent = TRUE)

    ## absolute sum
    maSum <- function (y, N = 5L, pad = NULL) {
        N <- as.integer(N)
        stopifnot(N > 0L)
        n <- length(y)
        ss <- cumsum(abs(y))
        ss[N:n] <- ss[N:n] - c(0L, ss[1L:(n - N)])
        if (!is.null(pad) & N > 1L)
            ss[1L:(N - 1L)] <- pad
        ss
    }
    N <- 1
    checkEquals(runStats("abssum", y, N)[-seq(N)],
                maSum(y, N)[-seq(N)])
    N <- 25
    checkEquals(runStats("abssum", y, N)[-seq(N)],
                maSum(y, N)[-seq(N)])

}

