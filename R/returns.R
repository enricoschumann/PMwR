returns <- function(x, ...)
    UseMethod("returns")

returns.NAVseries <- function(x, period = NULL, complete.first = TRUE,
                              pad = NULL, position = NULL, lag = 1, ...)
    returns(as.zoo(x), period = period,
            complete.first = complete.first,
            pad = pad, position, lag = lag, ...)

returns.zoo <- function(x, period = NULL, complete.first = TRUE,
                        pad = NULL, position = NULL, lag = 1, ...) {

    t <- time(x)
    x <- coredata(x)

    if (!is.null(period)) {
        returns(x, t = t, period = period,
                complete.first = complete.first,
                pad = pad, position = position, lag = lag, ...)
    } else {
        ans <- returns(x, period = NULL,
                       complete.first = complete.first,
                       pad = pad, position = position, lag = lag, ...)
        if (!is.null(pad))
            zoo(ans, t)
        else
            zoo(ans, t[-1L])
    }
}

returns.data.frame <- function(x, t = NULL, period = NULL, complete.first = TRUE,
                               pad = NULL, position = NULL,
                               weights = NULL, rebalance.when = NULL,
                               lag = 1, ...) {

    ans <- returns.default(x,
                           t = t, period = period,
                           complete.first = complete.first,
                           pad = pad, position = position,
                           weights = weights,
                           rebalance.when = rebalance.when,
                           lag = lag, ...)
    as.data.frame(ans)
}

returns.default <- function(x, t = NULL, period = NULL, complete.first = TRUE,
                            pad = NULL, position = NULL,
                            weights = NULL,
                            rebalance.when = NULL,
                            lag = 1, ...) {

    if (is.null(t) && is.character(period)) {
        warning("no timestamp information available, so ",
                sQuote("period"), " is ignored")
        period <- NULL
    }
    
    if (is.null(t) &&  is.null(position) && is.null(weights)) {
        .returns(x, pad = pad, lag = lag)
    } else if (is.null(t) &&  is.null(position) && !is.null(weights)) {
        returns_rebalance(prices = x, weights = weights,
                          when = rebalance.when, pad = pad)
    } else if (!is.null(t)) {
        if (is.unsorted(t)) {
            idx <- order(t)
            t <- t[idx]
            x <- x[idx]
        }
        if (!is.null(dim(x)) && min(dim(x)) > 1L)
        if (lag != 1L)
            warning(sQuote("lag"), " is ignored")
        pReturns(x, t, period, complete.first, pad = pad)
    } else {
        if (lag != 1L)
            warning(sQuote("lag"), " is ignored")
        twReturns(x, position, pad = pad)
    }
}

.returns <- function(x, pad = NULL, lag) {
    n <- NROW(x)
    if (n < 2L)
        stop("less than two observations")
    do.pad <- !is.null(pad)
    a <- 1:lag
    b <- n:(n-lag+1L)
    if (is.null(dim(x))) {
        x <- as.vector(x)
        rets <- x[-a] / x[-b] - 1
        if (do.pad)
            rets <- c(rep.int(pad, lag), rets)
    } else {
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
    x <- as.matrix(x)
    if (grepl("^ann", period, ignore.case = TRUE)) {
        xi <- as.Date(t)
        lx <- length(xi)
        t <- as.numeric(xi[lx] - xi[1L])/365
        ans <- if (t < 1 && !(grepl("!$", period))) {
            (x[lx]/x[1L]) - 1
               } else {
                   (x[lx]/x[1L])^(1/t) - 1
               }
        attr(ans, "period") <- "annualised"
        attr(ans, "t") <- c(xi[1L], xi[lx])
    } else if (tolower(period) == "itd") {
        lx <- length(x)
        (x[lx]/x[1L]) - 1
        attr(ans, "period") <- "itd"
        attr(ans, "t") <- if (is.null(t)) NULL else c(t[1], t[lx])
    } else if (tolower(period) == "ytd") {
        years <- as.numeric(format(t, "%Y"))
        i <- which(years < max(years))
        i <- if (!length(i)) 1 else max(i) 
        ii <- c(i, length(t))
        ans <- drop(returns(x[ii, ], pad = pad))
        attr(ans, "t") <- if (is.null(pad)) t[ii][-1L] else t[ii]
        attr(ans, "period") <- "ytd"
    } else if (tolower(period) == "mtd") {
        ymon <- as.numeric(format(t, "%Y%m"))
        i <- which(ymon < max(ymon))
        i <- if (!length(i)) 1 else max(i) 
        ii <- c(i, length(t))
        ans <- drop(returns(x[ii, ], pad = pad))
        attr(ans, "t") <- if (is.null(pad)) t[ii][-1L] else t[ii]
        attr(ans, "period") <- "mtd"
    } else {
        if (length(period) > 1L) {
            by <- period
        } else if (grepl("da(y|i)", period, ignore.case = TRUE)) {
            by <- format(t, "%Y%m%d")
            period <- "daily"
        } else if (grepl("year(ly)?", period, ignore.case = TRUE)) {
            by <- format(t, "%Y")
            period <- "yearly"
        } else if (grepl("month(ly)?", period, ignore.case = TRUE)) {
            by <- format(t, "%Y%m")
            period <- "monthly"
        } else {
            stop("unknown ", sQuote("period"))
        }
        ii <- last(x, by, TRUE)
        if (complete.first && by[1L] == by[2L])
            ii <- c(1, ii)

        ans <- returns(x[ii, ], pad = pad)
        attr(ans, "t") <- if (is.null(pad)) t[ii][-1L] else t[ii]
        attr(ans, "period") <- period
    }
    class(ans) <- "p_returns"
    ans
}

as.zoo.p_returns <- function (x, ...) {
    ## TODO: if list, warning
    zoo(x, attr(x,"t"))
}

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

.mtab <- function(x, ytd = "YTD", month.names = NULL, zero.print = "0", plus = FALSE,
                 digits = 1) {
    years <- as.numeric(format(attr(x, "t"), "%Y"))
    mons  <- as.numeric(format(attr(x, "t"), "%m"))
    tb <- array("", dim = c(length(unique(years)), 13L))
    tb[cbind(years - years[1L] + 1L, mons)] <- fmt(x, plus, digits)
    for (y in sort(unique(years)))
        tb[y - years[1L] + 1L, 13L] <- fmt(prod(x[years==y & !is.na(x)] + 1L) - 1L,
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

## not exported
print.p_returns <- function(x, ..., year.rows = TRUE,
                           month.names = NULL, zero.print = "0", plus = FALSE,
                           digits = 1) {
    if (is.list(x))
        warning("format of 'p_returns' objects has changed: see ChangeLog 2015-06-26")
    period <- attr(x, "period")
    timestamp <- attr(x, "t")
    if (period == "monthly" && is.null(dim(x))) {
        if (year.rows)
            print(.mtab(x, ytd = "YTD", month.names = month.names,
                       zero.print = zero.print, plus = plus, digits = digits),
                  quote = FALSE, print.gap = 1, right = TRUE)
        else
            print(t(.mtab(x, ytd = "YTD", month.names = month.names,
                       zero.print = zero.print, plus = plus, digits = digits)),
                  quote = FALSE, print.gap = 2, right = TRUE)
    } else if (period == "monthly") {
        tmp <- x
        tmp <- format(round(tmp*100,1), nsmall = digits)
        row.names(tmp) <- as.character(timestamp)
        print(unclass(tmp), quote = FALSE, print.gap = 2)
    } else if (period == "yearly") {
        tmp <- x
        names(tmp) <- format(timestamp, "%Y")
        if (year.rows)
            print(fmt(tmp, plus, digits), quote = FALSE)
        else
            print(as.matrix(fmt(tmp, plus, digits)), quote = FALSE)
    } else if (period == "annualised") {
        cat(format(round(x*100,1), nsmall = 1), "% p.a. ",
            "  [", format(timestamp[1], "%d %b %Y"), " -- ",
                 format(timestamp[2], "%d %b %Y"), "", sep = "")
        if (as.numeric(timestamp[2L]-timestamp[1L])/365 < 1)
            cat(", less than one year]\n", sep = "") else
            cat("]\n", sep = "")
    } else if (period == "ytd") {
        cat(paste(format(round(x * 100, digits), nsmall = digits), "%", 
            "  [", format(tail(timestamp,1), "%d %b %Y"), "]"), 
            sep = "\n")        
    } else if (period == "mtd") {
        cat(paste(format(round(x * 100, digits), nsmall = digits), "%", 
            "  [", format(tail(timestamp,1), "%d %b %Y"), "]"), 
            sep = "\n")        
    } else {
        print(unclass(x))
    }
    invisible(x)
}

## not exported
toLatex.p_returns <- function(object, ..., year.rows = TRUE,
                             ytd = "YTD", month.names = NULL, eol = "\\\\",
                              stand.alone = FALSE) {
    ## TODO: if list, warning
    period <- attr(object, "period")
    if (grepl("month(ly)?", period, ignore.case = TRUE)) {
        if (year.rows)
            mt <- .mtab(object, ytd = ytd, month.names = month.names)
        else
            mt <- t(.mtab(object, ytd = ytd, month.names = month.names))
    } else {
        stop("currently only supported for period ", sQuote("month"))
    }
    mt <- rbind(colnames(mt), mt)
    mt <- cbind(rownames(mt), mt)
    mt <- paste(apply(mt, 1, function(x) paste(x, collapse = "&")), eol)
    class(mt) <- "Latex"
    mt
}

## not exported
toHTML.p_returns <- function(x, ..., year.rows = TRUE,
                            ytd = "YTD", month.names = NULL,
                            stand.alone = TRUE,
                            table.style = NULL,
                            table.class = NULL,
                            th.style = NULL,
                            th.class = NULL,
                            td.style = "text-align:right;padding:0.5em;",
                            td.class = NULL,
                            tr.style = NULL, tr.class = NULL) {

    ## TODO: if list, warning
    period <- attr(x, "period")

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

    if (grepl("month(ly)?", period)) {
        if (year.rows)
            mt <- .mtab(x, ytd = ytd, month.names = month.names)
        else
            mt <- t(.mtab(x, ytd = ytd, month.names = month.names))
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

## not exported
toText.p_returns <- function(x, ..., year.rows = TRUE,
                            ytd = "YTD", month.names = NULL) {

    ## TODO: if list, warning
    period <- attr(x, "period")

    if (grepl("month(ly)?", period)) {
        if (year.rows)
            mt <- .mtab(x, ytd = ytd, month.names = month.names)
        else
            mt <- t(.mtab(x, ytd = ytd, month.names = month.names))
    } else {
        stop("currently only supported for period ", sQuote("month"))
    }
    mt <- rbind(colnames(mt), mt)
    mt <- cbind(rownames(mt), mt)
    mt <- unname(mt)
    mt <- apply(mt, 2, function(x) {format(x,
                                           width = max(5, nchar(x)),
                                           justify = "right")})
    mt <- apply(mt, 1, paste0, collapse = "")
    class(mt) <- "text"
    mt
}

## not exported
returns_rebalance <- function(prices, weights, when = NULL, pad = NULL) {
    nr <- nrow(prices)

    if (is.null(dim(prices)))
        stop("prices needs to be a matrix")

    if (nr < 2L)
        stop("less than 2 rows in prices: cannot compute returns")

    if (is.null(dim(weights)) && is.null(when)) {
        ## TODO faster implementation
    }

    if (is.null(dim(weights)))
        weights <- array(1, dim = c(nr, 1)) %*% weights

    if (dim(weights)[2L] != ncol(prices))
        warning("length of weights does not match number of price series")

    if (is.null(when) || isTRUE(when))
        when <- rep(TRUE, nr)

    if (is.numeric(when)) { ## cases such as '1,4,12,23,...'
        tmp <- logical(nr)
        tmp[round(when)] <- TRUE
        when <- tmp
    }
        
    val <- numeric(nr)
    h <- ctb <- array(0, dim = dim(prices))

    val[1] <- 1
    h[1, ] <- weights[1, ]/prices[1, ]

    for (i in 2:nr) {
        val[i] <- sum(prices[i, ]*h[i - 1, ])
        ctb[i,] <- (prices[i,]-prices[i-1,]) * h[i-1,] / val[i-1]
        if (when[i])
            h[i, ] <- val[i] * weights[i, ]/prices[i, ]
        else
            h[i, ] <- h[i - 1, ]
    }
    ans <- returns(val, pad = pad)
    attr(ans, "holdings") <- h
    attr(ans, "contributions") <- ctb
    ans
}


## not exported
returns_rebalance2 <- function(prices, weights, when = NULL, pad = NULL) {
    nr <- nrow(prices)
    pos <- array(NA, dim = dim(prices))
    weights <- array(NA, dim = dim(prices))
    ## ans
}


if (FALSE) {
    prices <- c(100 ,102 ,104 ,104 ,104.5 ,
                2   ,2.2 ,2.4 ,2.3 ,2.5   ,
                3.5 ,3   ,3.1 ,3.2 ,3.1)

    dim(prices) <- c(5,3)
    weights <- c(1,0,0)
    when <- as.logical(c(1,0,1,0,1))
    when <- as.logical(c(1,1,1,1,1))
    when <- TRUE

    ## TESTS
    weights <- c(1,0,0)
    all.equal(c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0)),
              c(returns(prices[,1],pad=0)))
    weights <- c(0,1,0)
    all.equal(c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0)),
              c(returns(prices[,2],pad=0)))
    weights <- c(0,0,1)
    all.equal(c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0)),
              c(returns(prices[,3],pad=0)))
    weights <- c(.5,.4,.1)
    all.equal(c(returns(prices,pad=0) %*% weights),
              c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0)))

    when <- as.logical(c(1,0,0,0,0))
    !isTRUE(all.equal(
        c(returns(prices,pad=0) %*% weights),
        c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0))))
}
