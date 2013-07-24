## not exported
returns0 <- function(x, pad = NULL) {
    n <- NROW(x)
    do.pad <- !is.null(pad)
    if (is.null(dim(x))) {
        x <- as.vector(x)
        rets <- x[-1L]/x[-n] - 1
        if (do.pad)
            rets <- c(pad, rets)
    } else {
        x <- as.matrix(x)
        rets <- x[2:n, ] / x[seq_len(n - 1L), ] - 1
        if (do.pad)
            rets <- rbind(pad, rets)
    }
    rets
}

## not exported
twReturns <- function(price, position, pad = NULL) {
    do.pad <- !is.null(pad)        
    position <- as.matrix(position)
    price <- as.matrix(price)
    n <- dim(price)[1L]
    ap <- position*price
    rt <- returns(price)        
    weights <- (price*position/rowSums(price*position))[-n, , drop = FALSE]
    rt <- rowSums(rt*weights)        
    if (do.pad) 
        rt <- c(pad, rt)
    rt
}

## not exported
pReturns <- function(x, t = NULL, period = "month",
                     complete.first = TRUE) {

    ## TODO: make internal function that checks x and t
    if (is.null(t)) {
        if (!inherits(x, "zoo")) {
            stop(sQuote("t"), " not supplied, so ", sQuote("x"),
                 " must inherit from ", sQuote("zoo"))
        } else {
            t <- index(x)
            x <- coredata(x)
        }
    } else {
        if (is.unsorted(t)) {
            idx <- order(t)
            t <- t[idx]
            x <- x[idx]
        }
    }
    if (grepl("da(y|i)", period, ignore.case = TRUE)) {
        by <- format(t, "%Y%m%d")
    } else if (grepl("month", period, ignore.case = TRUE)) {
        by <- format(t, "%Y%m")
    }
    ii <- PMwR:::last(x, by, TRUE)
    if (complete.first && by[1L] == by[2L])
        ii <- c(1, ii)

    ans <- list(returns = returns(x[ii]),
                t = t[ii][-1L],
                period = period)
    class(ans) <- "preturns"
    ans
}

returns <- function(x, t = NULL, period = "month", complete.first = TRUE,
                     pad = NULL, position) {
    if (is.null(t) &&  missing(position)) {
        returns0(x, pad)        
    } else if (!is.null(t)) {
        pReturns(x, t, period, complete.first)
    } else {
        twReturns(x, position, pad)
    }
}

## not exported
mtab <- function(x) {
    f <- function(x)
        format(round(100*x, 1), nsmall = 1)    
    years <- as.numeric(format(x$t, "%Y"))
    mons  <- as.numeric(format(x$t, "%m"))
    tb <- array("", dim = c(length(unique(years)), 13L))
    tb[cbind(years - years[1L] + 1L, mons)] <- f(x$returns)
    for (y in sort(unique(years)))
        tb[y - years[1L] + 1L, 13L] <- f(prod(x$returns[years==y] + 1L) - 1L)
    rownames(tb) <- sort(unique(years))
    colnames(tb) <- c(format(as.Date(paste0("2012-", 1:12, "-1")), "%b"), "YTD")
    tb
}

## TODO: tolatex
## paste(apply(tb, 1, function(x) paste(x, collapse = "&")), "\\\\")


print.preturns <- function(x, ..., year.rows = TRUE) {
    if (x$period == "month") {
        if (year.rows)
            print(mtab(x), quote = FALSE, print.gap = 2, right = TRUE)
        else
            print(t(mtab(x)), quote = FALSE, print.gap = 2, right = TRUE)
        
    } else {
        print(unclass(x))
    }
    invisible(x)
}
