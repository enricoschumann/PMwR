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

    M <- price * position
    rsM <- rowSums(M)
    weights <- (M/rsM)
    weights[abs(rsM) < 1e-14] <- 0
    weights <- weights[-n, , drop = FALSE]
    rt <- rowSums(rt * weights)        
    if (do.pad) 
        rt <- c(pad, rt)
    rt
}

## not exported
pReturns <- function(x, t, period, complete.first = TRUE, pad = NULL) {
    
    if (is.null(period)) {
        ans <- list(returns = returns(x, pad = pad),
                    t = if (is.null(pad)) t[-1L] else t,
                    period = period)        
    } else {        

        if (length(period) > 1L) {
            by <- period
        } else if (grepl("da(y|i)", period, ignore.case = TRUE)) {
            by <- format(t, "%Y%m%d")
        } else if (grepl("year(ly)?", period, ignore.case = TRUE)) {
            by <- format(t, "%Y")
        } else if (grepl("month(ly)?", period, ignore.case = TRUE)) {
            by <- format(t, "%Y%m")
        } 
        
        ii <- last(x, by, TRUE)
        if (complete.first && by[1L] == by[2L])
            ii <- c(1, ii)
        
        ans <- list(returns = returns(x[ii], pad = pad),
                    t = if (is.null(pad)) t[ii][-1L] else t[ii],
                    period = period)        
    }
    class(ans) <- "preturns"
    ans
}

returns <- function(x, t = NULL, period = NULL, complete.first = TRUE,
                    pad = NULL, position) {

    ## TODO: make internal function that checks x and t
    if (is.null(t)) {
        if (inherits(x, "zoo")) {
            t <- index(x)
            x <- coredata(x)
            if (!is.null(dim(x)))
                stop("with ", sQuote("t"), " supplied, ",
                     sQuote("x"), " must be a vector")                    
        }
    } else {
        if (!is.null(dim(x)))
            stop("with ", sQuote("t"), " supplied, ",
                 sQuote("x"), " must be a vector")                
        if (is.unsorted(t)) {
            idx <- order(t)
            t <- t[idx]
            x <- x[idx]
        }
    }

    if (is.null(t) &&  missing(position)) {
        returns0(x, pad = pad)        
    } else if (!is.null(t)) {
        pReturns(x, t, period, complete.first, pad = pad)
    } else {
        twReturns(x, position, pad = pad)
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



print.preturns <- function(x, ..., year.rows = TRUE) {
    if (!is.null(x$period) && grepl("month(ly)?", x$period)) {
        if (year.rows)
            print(mtab(x), quote = FALSE, print.gap = 2, right = TRUE)
        else
            print(t(mtab(x)), quote = FALSE, print.gap = 2, right = TRUE)
        
    } else {
        print(unclass(x))
    }
    invisible(x)
}

toLatex.preturns <- function(object, ..., year.rows = TRUE) {
    if (grepl("month(ly)?", object$period)) {
        if (year.rows)
            mt <- mtab(object)
        else
            mt <- t(mtab(object))
    } else {
        stop("currently only supported for period ", sQuote("month"))
    }
    mt <- rbind(colnames(mt), mt)
    mt <- cbind(rownames(mt), mt)
    mt <- paste(apply(mt, 1, function(x) paste(x, collapse = "&")), "\\\\")        
    class(mt) <- "Latex"
    mt
}

toHTML.preturns <- function(x, ..., year.rows = TRUE, stand.alone = TRUE) {

    .th <- function(x)
        paste0("<th>", x, "</th>")
    .td <- function(x)
        paste0("<td style='text-align:right;padding:0.5em;' >", x, "</td>")
    .tr <- function(x)
        paste0("<tr>", x, "</tr>")
    
    if (grepl("month(ly)?", x$period)) {
        if (year.rows)
            mt <- mtab(x)
        else
            mt <- t(mtab(x))
    } else {
        stop("currently only supported for period ", sQuote("month"))
    }
    mt <- rbind(colnames(mt), mt)
    mt <- cbind(rownames(mt), mt)
    mt <- unname(mt)
    mt[1, ]   <- .th(mt[1, ])
    mt[-1 ,1] <- .th(mt[-1, 1])
    mt[-1,-1] <- .td(mt[-1,-1])


    mt <- paste(.tr(apply(mt, 1, paste, collapse = "")), collapse = "")
    mt <- paste("<table>", mt, "</table>", sep = "")
    mt
}
