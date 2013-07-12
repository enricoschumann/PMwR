makeHHMMSS <- function(x, label = "time specification (HHMMSS)") {
    x <- as.character(x)
    if (nchar(x) == 1L)
        x <- paste("0", x, "0000", sep = "")
    if (nchar(x) == 2L)
        x <- paste(x, "0000", sep = "")
    if (nchar(x) == 4L)
        x <- paste(x, "00", sep = "")

    ss <- substr(x, 1, 2)
    if (ss > "24" || ss < "00")
        stop("check ", label)
    ss <- substr(x, 3, 4)
    if (ss > "60" || ss < "00")
        stop("check ", label)
    ss <- substr(x, 5, 6)
    if (ss > "60" || ss < "00")
        stop("check ", label)
    x
}

wait <- function(x) 
    if (length(x) == 1L)
        Sys.sleep(x) else
           Sys.sleep(runif(1L, min(x), max(x)))

last <- function(x, by, index = FALSE) {
    lby <- length(by)
    rby <- by[lby:1L]
    if (index)
        lby - match(unique(by), rby) + 1L
    else
        x[lby - match(unique(by), rby) + 1L]
}

first <- function(x, by, index = FALSE) {
    if (index)
        match(unique(by), by)
    else
        x[match(unique(by), by)]
}

x <- c(9,9,10,8,10,11)
div <- 2
t <- 4

divAdjust <- function(x, t, div, backward = TRUE, additive = FALSE) {
    if (!is.null(dim(x)))
        stop(sQuote("x"), " must be a vector")
    tmp <- t > 1L
    div <- div[tmp]
    t <- t[tmp]
    if (length(t) > 1L && length(div) == 1L)
        div <- rep(div, length(t))
    else if (length(div) != length(t))
        stop("different lengths for ", sQuote("div"),
             " and ", sQuote("t"))
    n <- length(x)
    if (!additive) {
        rets <- c(0, x[-1L]/x[-n] - 1)
        rets[t] <- (x[t] + div)/x[t - 1L] - 1
        new.series <- x[1L] * cumprod(1 + rets)        
        if (backward)
            new.series <- new.series * x[n] / new.series[n]
    } else {
        dif <- c(0, x[-1L] - x[-n])
        dif[t] <- dif[t] + div
        new.series <- x[1L] + cumsum(dif)                
        if (backward)
            new.series <- new.series - new.series[n] + x[n]
    }
    new.series        
}

letter2month <- function(s){
    s <- toupper(s)
    meaning <- c("C 1", "C 2", "C 3", "C 4",  "C 5",  "C 6",
                 "C 7", "C 8", "C 9", "C 10", "C 11", "C 12",
                 "P 1", "P 2", "P 3", "P 4",  "P 5" , "P 6",
                 "P 7", "P 8", "P 9", "P 10", "P 11", "P 12")
    meaning[match(s, LETTERS[1:24])]
}

insert <- function(x, list, values) {
    len <- length(list) * (length(values) - 1) + length(x)
    ans <- vector(typeof(x), length = len)
    seq_len(len)
}

