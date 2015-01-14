returns <- function(x, ...)
    UseMethod("returns")

returns.zoo <- function(x, ..., period = NULL, complete.first = TRUE,
                    pad = NULL, position, lag = 1) {

    t <- time(x)
    x <- coredata(x)

    if (!is.null(period)) {
        returns(x, ..., t = t, period = period,
                       complete.first = complete.first,
                       pad = pad, position, lag = lag)
        
    } else {
        ans <- returns(x, ..., period = NULL,
                complete.first = complete.first,
                pad = pad, position, lag = lag)
        if (!is.null(pad))
            zoo(ans, t) else zoo(ans, t[-1L])
    }
}

returns.default <- function(x, ..., t = NULL, period = NULL, complete.first = TRUE,
                    pad = NULL, position, lag = 1) {

    if (is.null(t) &&  missing(position)) {
        returns0(x, pad = pad, lag = lag)        
    } else if (!is.null(t)) {
        if (is.unsorted(t)) {
            idx <- order(t)
            t <- t[idx]
            x <- x[idx]
        }
        if (!is.null(dim(x)) && min(dim(x)) > 1L)
            stop("with ", sQuote("t"), " supplied, ",
                 sQuote("x"), " must be a vector")                

        if (lag != 1L)
            warning(sQuote("lag"), " is ignored")
        pReturns(x, t, period, complete.first, pad = pad)
    } else {
        if (lag != 1L)
            warning(sQuote("lag"), " is ignored")
        twReturns(x, position, pad = pad)
    }
}


## not exported
returns0 <- function(x, pad = NULL, lag) {
    n <- NROW(x)
    do.pad <- !is.null(pad)
    a <- 1:lag
    b <- n:(n-lag+1L)
    if (is.null(dim(x))) {
        x <- as.vector(x)
        rets <- x[-a] / x[-b] - 1
        if (do.pad)
            rets <- c(rep.int(pad, lag), rets)
    } else {
        x <- as.matrix(x)
        rets <- x[-a, ,drop = FALSE] / x[-b, ,drop = FALSE] - 1
        if (do.pad)
            rets <- do.call("rbind",
                            c(as.list(rep(pad, lag)), list(rets)))
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
    } else if (grepl("annuali(s|z)ed?", period, ignore.case = TRUE)) {
        stop("not supported yet")
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

as.zoo.preturns <- function (x, ...)
    zoo(x$returns, x$t)


## not exported
fmt <- function(x, plus, digits) {
    ans <- format(round(100*x, digits),
                  nsmall = if (digits < 0) 0 else digits,
                  trim = TRUE)
    if (plus)
        paste0(ifelse(x >= 0, "+", ""), ans)
    else
        ans
}

## not exported
mtab <- function(x, ytd = "YTD", month.names = NULL, zero.print = "0", plus = FALSE,
                 digits = 1) {
    years <- as.numeric(format(x$t, "%Y"))
    mons  <- as.numeric(format(x$t, "%m"))
    tb <- array("", dim = c(length(unique(years)), 13L))
    tb[cbind(years - years[1L] + 1L, mons)] <- fmt(x$returns, plus, digits)
    for (y in sort(unique(years)))
        tb[y - years[1L] + 1L, 13L] <- fmt(prod(x$returns[years==y] + 1L) - 1L,
                                           plus, digits)
    rownames(tb) <- sort(unique(years))
    colnames(tb) <- if (is.null(month.names))
                        c(format(as.Date(paste0("2012-", 1:12, "-1")), "%b"), ytd)
                    else
                        c(month.names, ytd)
    if (zero.print != "0")
        tb <- gsub("^ *[+-]?[0.]+$", zero.print, tb)
    tb
}

print.preturns <- function(x, ..., year.rows = TRUE,
                           month.names = NULL, zero.print = "0", plus = FALSE,
                           digits = 1) {
    if (!is.null(x$period) && grepl("month(ly)?", x$period, ignore.case = TRUE)) {
        if (year.rows)
            print(mtab(x, ytd = "YTD", month.names = month.names,
                       zero.print = zero.print, plus = plus, digits = digits),
                  quote = FALSE, print.gap = 1, right = TRUE)
        else
            print(t(mtab(x, ytd = "YTD", month.names = month.names,
                       zero.print = zero.print, plus = plus, digits = digits)),
                  quote = FALSE, print.gap = 2, right = TRUE)
        
    } else if (!is.null(x$period) && grepl("year(ly)?", x$period, ignore.case = TRUE)) {
        tmp <- x$returns
        names(tmp) <- format(x$t, "%Y")
        if (year.rows) 
            print(fmt(tmp, plus, digits), quote = FALSE)
        else 
            print(as.matrix(fmt(tmp, plus, digits)), quote = FALSE)
    } else {
        print(unclass(x))
    }
    invisible(x)
}

toLatex.preturns <- function(object, ..., year.rows = TRUE,
                             ytd = "YTD", month.names = NULL, eol = "\\\\") {
    if (grepl("month(ly)?", object$period, ignore.case = TRUE)) {
        if (year.rows)
            mt <- mtab(object, ytd = ytd, month.names = month.names)
        else
            mt <- t(mtab(object, ytd = ytd, month.names = month.names))
    } else {
        stop("currently only supported for period ", sQuote("month"))
    }
    mt <- rbind(colnames(mt), mt)
    mt <- cbind(rownames(mt), mt)
    mt <- paste(apply(mt, 1, function(x) paste(x, collapse = "&")), eol)        
    class(mt) <- "Latex"
    mt
}

toHTML.preturns <- function(x, ..., year.rows = TRUE,
                            ytd = "YTD", month.names = NULL,
                            stand.alone = TRUE,
                            table.style = NULL,
                            table.class = NULL,
                            th.style = NULL,
                            th.class = NULL,
                            td.style = "text-align:right;padding:0.5em;",
                            td.class = NULL,
                            tr.style = NULL, tr.class = NULL) {

    .ctag <- function(value, tag)
        if (!is.null(value))
            paste0(" ", tag, "=", value)
        else
            character(0L)
    
    .th <- function(x, style = th.style, class = th.class){
        open <- paste0("<th", .ctag(style, "style"), .ctag(class, "class"), ">")
        paste0(open, x, "</th>")
    }
    .td <- function(x, style = td.style, class = td.class){
        open <- paste0("<td", .ctag(style, "style"), .ctag(class, "class"), ">")
        paste0(open, x, "</td>")
    }
    .tr <- function(x, style = tr.style, class = tr.class){
        open <- paste0("<tr", .ctag(style, "style"), .ctag(class, "class"), ">")
        paste0(open, x, "</tr>")
    }
    
    if (grepl("month(ly)?", x$period)) {
        if (year.rows)
            mt <- mtab(x, ytd = ytd, month.names = month.names)
        else
            mt <- t(mtab(x, ytd = ytd, month.names = month.names))
    } else {
        stop("currently only supported for period ", sQuote("month"))
    }
    mt <- rbind(colnames(mt), mt)
    mt <- cbind(rownames(mt), mt)
    mt <- unname(mt)
    mt[1, ]   <- .th(mt[1, ])
    mt[-1 ,1] <- .th(mt[-1, 1])
    mt[-1,-1] <- .td(mt[-1,-1])

    mt <- .tr(apply(mt, 1, paste, collapse = ""))
    open <- paste0("<table", .ctag(table.style, "style"),
                             .ctag(table.class, "class"), ">")
    mt <- c(open, mt, "</table>")
    mt
}
