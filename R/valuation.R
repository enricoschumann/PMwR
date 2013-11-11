## valuation <- function(x, ...) {
##     UseMethod("valuation")
## }
## valuation.default <- function(x, ...) {
##     do.call(x, list(...))
## }
## valuation.instrument <- function(x, ...,
##                                  data = function(x, ...) list(...)) {

##     do.call(x$vfun, data(x, ...))
## }


value <- function(x, ...) {
    UseMethod("value")
}

value.default <- function(x, ...) {
    force(x)
}

value.journal <- function(x,
                          cashflow = function(x) x$amount * x$price,
                          multiplier = 1,                          
                          flip.sign = TRUE,
                          ...) {

    if (!is.null(names(multiplier)))
        multiplier <- multiplier[x$instrument]
    ans <- x
    ans$instrument <- "cash"
    ans$amount <- cashflow(x) * multiplier
    if (flip.sign)
        ans$amount <- ans$amount * -1    
    ans$price <- 1
    ans
}

value.position <- function(x, price.table, multiplier = 1, do.sum = FALSE, unit) {

    if (!is.null(names(multiplier)))
        multiplier <- multiplier[x$instrument]

    ans <- x$position * price.table
    if (any(multiplier != 1))
        ans <- ans %*% diag(multiplier, length(x$instrument))
    if (do.sum)
        ans <- rowSums(ans)
    ans
}

