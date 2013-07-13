## -*- truncate-lines: t; -*-
## Time-stamp: <2013-07-13 14:54:21 CEST (es)>

pm <- function(x, xp = 2, threshold = 0, lower = TRUE,
               keep.sign = FALSE, normalise = FALSE) {

    ##TODO: normalise: only if xp is integer?
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
        abs(x - round(x)) < tol

    x <- x - threshold
    if (lower)
        x <- x - abs(x)
    else
        x <- x + abs(x)        
    if (keep.sign)
        sx <- sign(x)
    x <- abs(x)

    if (xp == 1L)
        ans <- sum(x)/2/length(x)
    else if (xp == 2L)
        ans <- sum(x*x)/4/length(x)
    else if (xp == 3L)
        ans <- sum(x*x*x)/8/length(x)
    else if (xp == 4L)
        ans <- sum(x*x*x*x)/16/length(x)
    else 
        ans <- sum(x^xp)/2^xp/length(x)

    if (normalise)
        ans^(1/xp)
    else
        ans

}
