runStats <- function(what, y, N = 5L, pad = NULL, q = NULL, h = NULL) {
    N <- as.integer(N)
    y <- as.double(y)
    ny <- length(y) ## integer
    if (N <= 0L) {
        stop("'N' must be a postive integer")
    } else if (!ny) {
        stop("'y' must have a postive length")
    } else if (N > ny)
        stop("length of y must be greater than N")
    switch(what,
           mean = res <- .C("mA",    y = y, N = N, ny = ny, res = double(ny))$res,
           min =  res <- .C("mMin2", y = y, N = N, ny = ny, res = double(ny))$res,
           max =  res <- .C("mMax2", y = y, N = N, ny = ny, res = double(ny))$res,
           sum =  res <- .C("mSum",  y = y, N = N, ny = ny, res = double(ny))$res,
           sum2 =  res <- .Call("mSum2",  y = y, N = N),
           abssum = res <- .C("mAbsSum",  y = y, N = N, ny = ny, res = double(ny))$res,
           ## if no match
           stop("unknown 'what'") )
    if (!is.null(pad) && N > 1L)
        res[seq_len(N - 1L)] <- pad
    res
}

## mSum <- function (y, N = 5L, pad = NULL) {
##     N <- as.integer(N)
##     stopifnot(N > 0L)
##     n <- length(y)
##     ss <- cumsum(y)
##     ss[N:n] <- ss[N:n] - c(0L, ss[1L:(n - N)])
##     if (!is.null(pad) & N > 1L)
##         ss[1L:(N - 1L)] <- pad
##     ss
## }
