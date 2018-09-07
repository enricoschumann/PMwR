## -*- truncate-lines: t; -*-
## Copyright (C) 2008-18  Enrico Schumann

quote32 <- q32 <- function(price, sep = "(-|'|:)", warn = TRUE) {
    if (is.character(price)) {
        if (warn &&
            any(!grepl(paste0("[0-9]+", sep, "?[0-9]+"), price)))
            warning("(some) prices do not match pattern <number> <sep> <number>")

        price <- trimws(price)
        tmp <- strsplit(price, sep)
        handle <- unlist(lapply(tmp, `[[`, 1))

        ticks <- frac <- character(length(tmp))

        with_tf <- lengths(tmp) > 1L
        ticks_ <- substr(unlist(lapply(tmp[with_tf], `[[`, 2)), 1, 2)
        ticks[with_tf] <- ticks_
        ticks[is.na(ticks)| ticks == ""] <- "0"

        frac_ <- substr(unlist(lapply(tmp[with_tf], `[[`, 2)), 3, 3)
        frac[with_tf] <- frac_
        frac[is.na(frac)| frac == ""] <- "0"
        
        frac[frac == "0"] <- 0
        frac[frac == "2"] <- 1
        frac[frac == "5"] <- 2
        frac[frac == "+"] <- 2
        frac[frac == "7"] <- 3

        handle <- as.numeric(handle)
        ticks <- as.numeric(ticks)
        frac <- as.numeric(frac)
        ans <- handle + ticks/32 + frac/32/4
    } else {
        ans <- price    

        handle <- trunc(price)
        tmp <- price - handle
        ticks <- (tmp*128) %/% 4
        frac <- round(tmp*128-ticks*4)

        r.e <- abs(price - handle - ticks/32 - frac/4/32)
        if (warn && any(r.e > .Machine$double.eps^0.5))
            warning("largest rounding error is ", prettyNum(max(r.e)))
    }
    attr(ans, "handle") <- handle
    attr(ans, "ticks") <- ticks
    attr(ans, "fraction")  <- frac
    class(ans) <- "quote32"
    ans
}

print.quote32 <- function(x, sep = "-",
                          display = if (.Platform$OS.type == "unix")
                          "fractional" else "integer", ...) {

    fracsym <- if (display == "fractional")
                   c("0", "\u00bc", "+", "\u00be")
               else if (display == "integer")
                   c("0", "2", "+", "7")
               else
                   stop(sQuote("display"), " must be either ",
                        sQuote("fractional"), " or ",
                        sQuote("integer"))
    frac <- rep("", length(x))
    for (i in 0:3)
        frac[attr(x, "fraction") == i] <- fracsym[i+1]
    
    print.default(paste0(attr(x, "handle"), sep,
                         sprintf("%02d", attr(x, "ticks")),
                         frac, ""), quote = FALSE)
    invisible(x)
}

Ops.quote32 <- function (e1, e2) {
    e1 <- as.numeric(e1)
    e2 <- as.numeric(e2)
    NextMethod(.Generic)
}

`-.quote32` <- function(e1,e2) {
    ans <- as.numeric(e1) - as.numeric(e2)
    quote32(ans)
}

`+.quote32` <- function(e1, e2) {
    ans <- as.numeric(e1) + as.numeric(e2)
    quote32(ans)
}

c.quote32 <- function(..., recursive = FALSE) {
    L <- list(...)
    ans <- unlist(L)
    attr(ans, "handle") <- unlist(lapply(L, attr, "handle"))
    attr(ans, "ticks")  <- unlist(lapply(L, attr, "ticks"))
    attr(ans, "fraction") <- unlist(lapply(L, attr, "fraction"))
    class(ans) <- "quote32"
    ans
}

Summary.quote32 <- function(..., na.rm){
    if (.Generic %in% c("min", "max", "range")) {
        ans <- NextMethod(.Generic)
        quote32(ans)
    } else
        NextMethod(.Generic)
}

diff.quote32 <- function(x, ...) {
    ans <- x[2:length(x)] - x[1:(length(x)-1)]
    quote32(ans)
}
