## -*- truncate-lines: t; fill-column: 60; -*-

quote32 <- q32 <- function(price, sep = "-") {

    if (is.character(price)) {
        tmp <- strsplit(price, sep, fixed = TRUE)
        handle <- unlist(lapply(tmp, `[[`, 1))
        ticks <- substr(unlist(lapply(tmp, `[[`, 2)), 1, 2)
        frac <- substr(unlist(lapply(tmp, `[[`, 2)), 3,3)
        frac[frac == "0"] <- 0
        frac[frac == "2"] <- 1
        frac[frac == "5"] <- 2
        frac[frac == "7"] <- 3

        handle <- as.numeric(handle)
        ticks <- as.numeric(ticks)
        frac <- as.numeric(frac)
        ans <- handle + ticks/32 + frac/32
    } else {
        ans <- price    

        handle <- trunc(price)
        tmp <- price - handle
        ticks <- (tmp*128) %/% 4
        frac <- tmp*128-ticks*4

    }
    attr(ans, "handle") <- handle
    attr(ans, "ticks") <- ticks
    attr(ans, "fraction")  <- frac
    class(ans) <- "quote32"
    ans
}

print.quote32 <- function(x, sep = "-",
                          display = if (.Platform$OS.type == "unix")
                          "fractional" else "integer") {

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
    
    cat(paste0(attr(x, "handle"), sep,
               sprintf("%02d", attr(x, "ticks")),
               frac, "\n"), sep = "")
    invisible(x)
}

## `-.quote32` <- function(e1,e2) {
##     quote32(as.numeric(e1)-as.numeric(e2))        
## }
