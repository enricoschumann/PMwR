.timestamp <- function(x, ...) {
    UseMethod(".timestamp")
}

instrument <- instrument <- function(x, ...) {
    UseMethod("instrument")
}

instrument.position <- function(x, ...) {
    attr(x, "instrument")
}

instrument.pricetable <- function(x, ...) {
    attr(x, "instrument")
}

instrument.journal <- function(x, ...) {
    x$instrument
}

instrument.NAVseries <- function(x, ...) {
    attr(x, "instrument")
}


`instrument<-` <- function(x, ..., value) {
    UseMethod("instrument<-")
}

`instrument<-.journal` <- function(x, ..., value) {
    len <- length(value)
    lenx <- length(x)
    if (len == lenx)
        x$instrument <- value
    else if (len == 1L)
        x$instrument <- rep(value, lenx)
    else
        stop("length(instrument) differs from length(journal)")
    x
}

`instrument<-.NAVseries` <- function(x, ..., value) {
    attr(x, "instrument") <- value
    x
}

