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
           mean = res <- .C("mA",    y = y, N = N, ny = ny, res = double(ny),DUP = FALSE)$res,
           min =  res <- .C("mMin2", y = y, N = N, ny = ny, res = double(ny),DUP = FALSE)$res,
           max =  res <- .C("mMax2", y = y, N = N, ny = ny, res = double(ny),DUP = FALSE)$res,
           sum =  res <- .C("mSum",  y = y, N = N, ny = ny, res = double(ny),DUP = FALSE)$res,
           abssum = res <- .C("mAbsSum",  y = y, N = N, ny = ny, res = double(ny),DUP = FALSE)$res,
           ## if no match
           stop("unknown 'what'") )
    if (!is.null(pad) && N > 1L)
        res[seq_len(N - 1L)] <- pad
    res
}
