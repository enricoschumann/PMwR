## -*- truncate-lines: t; -*-
## Copyright (C) 2008-18  Enrico Schumann

returns <- function(x, ...)
    UseMethod("returns")

returns.default <- function(x, t = NULL, period = NULL,
                            complete.first = TRUE,
                            pad = NULL, position = NULL,
                            weights = NULL,
                            rebalance.when = NULL,
                            lag = 1, ...) {

    if (!is.null(position)) {
        position <- NULL
        warning("time-weighted returns not supported, so position is ignored")
    }

    ## if (is.null(period) && is.null(rebalance.when))
    ##     t <- NULL
    
    if (is.unsorted(t)) {  ## is.unsorted(NULL) == FALSE
        idx <- order(t)
        t <- t[idx]
        x <- x[idx]
    }
        
    if (is.character(rebalance.when)) {
        rebalance.when <- tolower(rebalance.when)
        if (rebalance.when == "endofmonth")
            rebalance.when  <- last(t, format(as.Date(t), "%Y-%m"), TRUE)
        else if (rebalance.when == "endofyear")
            rebalance.when  <- last(t, format(as.Date(t), "%Y"), TRUE)
        if (!is.null(period)) {
            warning("rebalance.when is specified, so period is ignored")
            period <- NULL
        }
    }
    if (!is.null(t) &&
        inherits(rebalance.when, class(t))) {
        ii <- match(rebalance.when, t)
        if (any(is.na(ii)))
            warning(sQuote("rebalance.when") ,
                    " does not match timestamp")
        rebalance.when <- ii[!is.na(ii)]        
    }

    if (is.null(t) &&
        is.character(period)) {
        warning("no timestamp information available, so ",
                sQuote("period"), " is ignored")
        period <- NULL
    }

    if (is.null(period) &&  is.null(position) && is.null(weights)) {
        .returns(x, pad = pad, lag = lag)
    } else if (is.null(position) && !is.null(weights)) {
        returns_rebalance(prices = x, weights = weights,
                          when = rebalance.when, pad = pad)
    } else if (!is.null(period)) {
        if (is.unsorted(t)) {
            idx <- order(t)
            t <- t[idx]
            x <- x[idx]
        }
        if (lag != 1L)
            warning(sQuote("lag"), " is ignored")
        pReturns(x, t, period, complete.first, pad = pad)
    } else {
        if (lag != 1L)
            warning(sQuote("lag"), " is ignored")
        warning("tw returns not supported any more")
        twReturns(x, position, pad = pad)
    }
}


## ---[Handling of 'timestamp' in methods]---
##
## Methods are responsible for 'stripping down' the
## input to x and t, calling 'returns.default' (or some
## other method) and then re-assembling the
## original class's structure. When there is no 'period'
## and no 'rebalance.when', methods should keep timestamp
## information for themselves and not pass it on.

returns.NAVseries <- function(x, period = NULL, complete.first = TRUE,
                              pad = NULL, position = NULL, lag = 1, ...) {

    ## does *not* return a NAVseries since it is not
    ## defined for returns, only for NAVs (levels)

    if (!is.null(period)) {
        returns.default(x, t = attr(x, "timestamp"), period = period,
                        complete.first = complete.first,
                        pad = pad, position = position, lag = lag, ...)
    } else {
        returns.default(c(x), period = NULL,
                        complete.first = complete.first,
                        pad = pad, position = position, lag = lag, ...)
    }
}

returns.zoo <- function(x, period = NULL, complete.first = TRUE,
                        pad = NULL, position = NULL,
                        weights = NULL, rebalance.when = NULL,
                        lag = 1, ...) {

    t <- time(x)
    x <- coredata(x)

    if (!is.null(period)) {
        returns.default(x, t = t, period = period,
                        complete.first = complete.first,
                        pad = pad, position = position,
                        weights = weights,
                        rebalance.when = rebalance.when,
                        lag = lag, ...)
    } else {
        ans <- returns.default(x, t = t, period = NULL,
                               complete.first = complete.first,
                               pad = pad, position = position,
                               weights = weights,
                               rebalance.when = rebalance.when,
                               lag = lag, ...)
        attrs <- attributes(ans)
        ans <- if (!is.null(pad))
                   zoo(ans, t)
               else
                   zoo(ans, t[-1L])
        attr(ans, "holdings") <- attrs$holdings
        attr(ans, "contributions") <- attrs$contributions
        ans
    }
}

returns.data.frame <- function(x, t = NULL, period = NULL,
                               complete.first = TRUE,
                               pad = NULL, position = NULL,
                               weights = NULL, rebalance.when = NULL,
                               lag = 1, ...) {

    ans <- returns.default(x, t = t, period = period,
                           complete.first = complete.first,
                           pad = pad, position = position,
                           weights = weights,
                           rebalance.when = rebalance.when,
                           lag = lag, ...)
    as.data.frame(ans)
}

.returns <- function(x, pad = NULL, lag) {
    n <- NROW(x)
    if (n < 2L)
        stop("fewer than two observations")
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
    ## TODO add also: 'previous month' and pattern 'YYYY-?MM'

    x <- as.matrix(x)
    if (!is.null(colnames(x)))
        instr <- colnames(x)
    else
        instr <- NULL
    nc <- ncol(x)
    period <- tolower(period)
    if (grepl("^ann", period, ignore.case = TRUE)) {
        if (!is.null(pad))
            warning(sQuote("pad"), " is ignored")
        force <- grepl("!$", period)
        ans <- numeric(nc)
        names(ans) <- instr
        is.ann  <- logical(nc)
        from.to <- array(NA, dim = c(nc, 2))
        colnames(from.to) <- c("from", "to")
        t <- as.Date(t)
        for (j in 1:nc) {
            xj <- x[, j]
            t1 <- max(which(!is.na(xj)))
            t0 <- min(which(!is.na(xj)))
            tt <- as.numeric( t[t1] - t[t0] )/365
            tmp <- xj[t1]/xj[t0]
            if (tt > 1 || force) {
                tmp <- tmp^(1/tt)
                is.ann[j] <- TRUE
            }
            ans[j] <- tmp - 1
            from.to[ j, ] <- c(t[t0], t[t1])
        }
        attr(ans, "period") <- if (force)
                                   "annualised!" else "annualised"
        class(from.to) <- "Date"
        attr(ans, "t") <- from.to
        attr(ans, "is.annualised") <- is.ann
    } else if (period == "itd") {
        if (!is.null(pad))
            warning(sQuote("pad"), " is ignored")
        ans <- numeric(nc)
        names(ans) <- instr
        from.to <- array(NA, dim = c(nc, 2))
        colnames(from.to) <- c("from", "to")
        for (j in 1:nc) {
            xj <- x[ ,j]
            t0 <- min(which(!is.na(xj)))
            t1 <- max(which(!is.na(xj)))
            ans[j] <- drop(returns( xj[c(t0, t1)] ))
            from.to[j,] <- c(t[t0], t[t1])
        }
        attr(ans, "period") <- "itd"
        class(from.to) <- "Date"
        attr(ans, "t") <- from.to
    } else if (grepl("^ytd", period, ignore.case = TRUE)) {
        ## TODO allow syntax like "ytd02-15"?
        ## => returns a vector of returns ytd
        ##    up to 15 Feb
        if (!is.null(pad))
            warning(sQuote("pad"), " is ignored")
        years <- as.numeric(format(t, "%Y"))
        if (period != "ytd!" && max(years) != as.numeric(format(Sys.Date(), "%Y")))
            warning("max. timestamp (", max(years), ") does not match current year")
        ans <- numeric(nc)
        names(ans) <- instr
        from.to <- array(NA, dim = c(nc, 2))
        colnames(from.to) <- c("from", "to")
        for (j in 1:nc) {
            xj <- x[ ,j]
            i <- which(years < max(years))
            if (!length(i)) {
                ## all obs are within one year
                i <- seq_along(xj)
                t0 <- min(which(!is.na(xj[i])))
            } else {
                t0 <- max(which(!is.na(xj[i])))
            }
            t1 <- max(which(!is.na(xj)))
            ans[j] <- drop(returns( xj[c(t0, t1)] ))
            from.to[j,] <- c(t[t0], t[t1])
        }
        class(from.to) <- "Date"
        attr(ans, "t") <- from.to
        attr(ans, "period") <- "ytd"
    } else if (grepl("^ytm", period, ignore.case = TRUE)) {
        if (!is.null(pad))
            warning(sQuote("pad"), " is ignored")
        ymon <- as.numeric(format(t, "%Y%m"))
        years <- as.numeric(format(t, "%Y"))
        if (period != "ytm!" && max(years) != as.numeric(format(Sys.Date(), "%Y")))
            warning("max. timestamp (", max(years), ") does not match current year")
        ans <- numeric(nc)
        from.to <- array(NA, dim = c(nc, 2))
        colnames(from.to) <- c("from", "to")
        for (j in 1:nc) {
            xj <- x[ , j]
            i <- which(years < max(years))
            if (!length(i)) {
                ## all obs are within one year
                i <- seq_along(xj)
                t0 <- min(which(!is.na(xj[i])))
            } else {
                t0 <- max(which(!is.na(xj[i])))
            }
            t1 <- max(which(!is.na(xj)))
            ans[j] <- drop(returns( xj[c(t0, t1)] ))
            from.to[j,] <- c(t[t0], t[t1])
        }
        class(from.to) <- "Date"
        attr(ans, "t") <- from.to
        attr(ans, "period") <- "ytd"
    } else if (period == "mtd") {
        if (!is.null(pad))
            warning(sQuote("pad"), " is ignored")
        ymon <- as.numeric(format(t, "%Y%m"))
        ans <- numeric(nc)
        names(ans) <- instr
        from.to <- array(NA, dim = c(nc, 2))
        colnames(from.to) <- c("from", "to")
        for (j in 1:nc) {
            xj <- x[ ,j]
            i <- which(ymon < max(ymon))
            if (!length(i)) {
                ## all obs are within one month
                i <- seq_along(xj)
                t0 <- min(which(!is.na(xj[i])))
            } else {
                t0 <- max(which(!is.na(xj[i])))
            }
            t1 <- max(which(!is.na(xj)))
            ans[j] <- drop(returns( xj[c(t0, t1)] ))
            from.to[j,] <- c(t[t0], t[t1])
        }
        class(from.to) <- "Date"
        attr(ans, "t") <- from.to
        attr(ans, "period") <- "mtd"
    } else if ((grepl("[0-9][0-9][0-9][0-9]", period, ignore.case = TRUE))) {
        m <- which(format(t, "%Y") == period)
        ii <- c(max(1, min(m)-1), max(m))
        ans <- returns(x[ii, ], pad = pad)
        attr(ans, "t") <- t(t[ii])
        class(attr(ans, "t")) <- "Date"
        attr(ans, "period") <- period

    } else {
        if (length(period) > 1L) {
            by <- period
        } else if (grepl("hour(ly)?", period, ignore.case = TRUE)) {
            by <- format(t, "%Y%m%d%H")
            period <- "hourly"
        } else if (grepl("da(y|i)", period, ignore.case = TRUE)) {
            by <- format(t, "%Y%m%d")
            period <- "daily"
        } else if (grepl("month(ly)?", period, ignore.case = TRUE)) {
            by <- format(t, "%Y%m")
            period <- "monthly"
        } else if (grepl("quarter(ly)?", period, ignore.case = TRUE)) {
            by <- paste0(format(t, "%Y"), "-", quarters(t))
            period <- "quarterly"
        } else if (grepl("year(ly)?", period, ignore.case = TRUE)) {
            by <- format(t, "%Y")
            period <- "yearly"
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
    if (period == "monthly")
        class(ans) <- c("p_returns_monthly", class(ans))

    ans
}

as.zoo.p_returns <- function (x, ...) {
    zoo(x, order.by = attr(x, "t"), ...)
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

## not exported
.mtab <- function(x, t, ytd = "YTD", month.names = NULL,
                  zero.print = "0", plus = FALSE, digits = 1,
                  na.print = NULL) {
    years <- as.numeric(format(t, "%Y"))
    mons  <- as.numeric(format(t, "%m"))
    tb <- array("", dim = c(length(unique(years)), 13L))
    tb[cbind(years - years[1L] + 1L, mons)] <- fmt(x, plus, digits)
    for (y in suy <- sort(unique(years)))
        tb[y - years[1L] + 1L, 13L] <- fmt(prod(x[years==y & !is.na(x)] + 1L) - 1L,
                                           plus, digits)
    rownames(tb) <- sort(unique(years))
    colnames(tb) <- if (is.null(month.names))
                        c(format(as.Date(paste0("2012-", 1:12, "-1")), "%b"), ytd)
                    else
                        c(month.names, ytd)
    if (!is.null(na.print))
        tb <- gsub("NA", na.print, tb, fixed = TRUE)
    if (zero.print != "0")
        tb <- gsub("^ *[+-]?[0.]+$", zero.print, tb)
    tb
}

## not exported
print.p_returns_monthly <- function(x, ..., year.rows = TRUE,
                           month.names = NULL, zero.print = "0", plus = FALSE,
                           digits = 1, na.print = NULL) {

    period <- attr(x, "period")
    timestamp <- attr(x, "t")

    if (is.null(dim(x))) {
        if (year.rows)
            print(.mtab(x, timestamp, ytd = "YTD", month.names = month.names,
                        zero.print = zero.print, plus = plus, digits = digits),
                  quote = FALSE, print.gap = 1, right = TRUE)
        else
            print(t(.mtab(x, timestamp, ytd = "YTD", month.names = month.names,
                       zero.print = zero.print, plus = plus, digits = digits)),
                  quote = FALSE, print.gap = 2, right = TRUE)
    } else {
        tmp <- x
        tmp <- format(round(tmp*100, 1), nsmall = digits)
        row.names(tmp) <- as.character(timestamp)
        print(unclass(tmp), quote = FALSE, print.gap = 2)
    }

    invisible(x)
}

print.p_returns <- function(x, ..., year.rows = TRUE,
                           month.names = NULL, zero.print = "0", plus = FALSE,
                           digits = 1, na.print = NULL) {
    period <- attr(x, "period")
    timestamp <- attr(x, "t")
    instr <- names(x)
    if (period == "yearly" && is.null(dim(x))) {
        tmp <- x
        names(tmp) <- format(timestamp, "%Y")
        if (year.rows)
            print(fmt(tmp, plus, digits), quote = FALSE)
        else
            print(as.matrix(fmt(tmp, plus, digits)), quote = FALSE)
    } else if (period == "yearly") {
        tmp <- unclass(x)
        rownames(tmp) <- format(timestamp, "%Y")
        attr(tmp, "t") <- NULL
        attr(tmp, "period") <- NULL
        if (year.rows) {
            print(fmt(tmp, plus, digits), quote = FALSE, print.gap = 1, right = TRUE)
        } else {
            print(fmt(t(tmp), plus, digits), quote = FALSE, print.gap = 1, right = TRUE)
        }
    } else if (grepl("annualised", period)) {
        if (!is.null(instr))
            nn <- paste0(format(instr,
                         width = max(nchar(instr)),
                         justify = "right"), ": ")
        else
            nn <- ""
        r_str <- paste0(format(round(x*100, digits), nsmall = digits), "%  ")
        cal_str <- paste0("[",
                       format(timestamp[,1],"%d %b %Y"), " -- ",
                       format(timestamp[,2],"%d %b %Y"))

        note <- rep("]", length(x))
        note[as.numeric(timestamp[,2L]-timestamp[,1L])/365 < 1 &
              attr(x, "is.annualised")] <-
            "; less than one year, but annualised]"
        note[as.numeric(timestamp[,2L]-timestamp[,1L])/365 < 1 &
              !attr(x, "is.annualised")] <-
            "; less than one year, not annualised]"
        cat(paste0(nn, r_str, cal_str, note, collapse = "\n"), "\n")
    } else if (period == "ytd" || period == "mtd" || period == "itd" ||
               grepl("^[0-9][0-9][0-9][0-9]$", period)) {
        if (!is.null(instr))
            nn <- paste0(format(instr,
                         width = max(nchar(instr)),
                         justify = "right"), ": ")
        else
            nn <- ""
        r_str <- paste0(format(round(x*100, digits), nsmall = digits), "%  ")
        cal_str <- paste0("[",
                          format(timestamp[,1],"%d %b %Y"), " -- ",
                          format(timestamp[,2],"%d %b %Y"),
                          "]")
        cat(paste0(nn, r_str, cal_str, collapse = "\n"), "\n")
    } else {
        print(unclass(x))
    }
    invisible(x)
}

## not exported
toLatex.p_returns <- function(object, ..., year.rows = TRUE,
                              ytd = "YTD", month.names = NULL, eol = "\\\\",
                              stand.alone = FALSE) {

    period <- attr(object, "period")
    timestamp <- attr(object, "t")

    if (grepl("month(ly)?", period, ignore.case = TRUE)) {
        if (year.rows)
            mt <- .mtab(object, timestamp, ytd = ytd, month.names = month.names, ...)
        else
            mt <- t(.mtab(object, timestamp, ytd = ytd, month.names = month.names, ...))
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
                             td.style = "text-align:right; padding:0.5em;",
                             td.class = NULL,
                             tr.style = NULL, tr.class = NULL,
                             browse = FALSE) {

    stop("currently only supported for period ", sQuote("month"))
}

toHTML.p_returns_monthly <- function(x, ..., year.rows = TRUE,
                                     ytd = "YTD", month.names = NULL,
                                     stand.alone = TRUE,
                                     table.style = NULL,
                                     table.class = NULL,
                                     th.style = NULL,
                                     th.class = NULL,
                                     td.style = "text-align:right; padding:0.5em;",
                                     td.class = NULL,
                                     tr.style = NULL, tr.class = NULL,
                                     browse = FALSE) {

    period <- attr(x, "period")
    timestamp <- attr(x, "t")

    .ctag <- function(value, tag)
        if (!is.null(value) && value != "")
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

    if (year.rows)
        mt <- .mtab(x, timestamp, ytd = ytd, month.names = month.names)
    else
        mt <- t(.mtab(x, timestamp, ytd = ytd, month.names = month.names))

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

    if (browse) {
        tmp <- tempfile("PMwR_", fileext = ".html")
        writeLines(mt, con= file(tmp), sep = "\n")
        browseURL(paste0("file://", tmp))
        invisible(mt)
    } else
        mt
}

## not exported
toText.p_returns <- function(x, ..., year.rows = TRUE,
                            ytd = "YTD", month.names = NULL) {

    period <- attr(x, "period")
    timestamp <- attr(x, "t")

    if (grepl("month(ly)?", period)) {
        if (year.rows)
            mt <- .mtab(x, timestamp, ytd = ytd, month.names = month.names)
        else
            mt <- t(.mtab(x, timestamp, ytd = ytd, month.names = month.names))
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
        ## TODO faster implementation?
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
    ans <- .returns(val, pad = pad, lag = 1L)
    attr(ans, "holdings") <- h
    attr(ans, "contributions") <- ctb
    ans
}

rc <- function(R, weights, timestamp, segments = NULL) {
    if (missing(weights))
        weights <- 1
    if (is.null(segments)) {
        segments <- if (!is.null(cr <- colnames(R)))
                        cr
                    else if (!is.null(cr <- colnames(weights)))
                        cr
                    else
                        paste0("segment_", 1:ncol(weights))
    }
    if (missing(timestamp))
        timestamp <- 1:nrow(R)
    ns <- length(segments)
    nt <- length(timestamp)
    df <- data.frame(timestamp,
                     cbind(weights*R, rowSums(weights*R)),
                     stringsAsFactors = FALSE)
    names(df) <- c("timestamp", segments, "total")

    later_r <- c(rev(cumprod(1 + rev(df[["total"]])))[-1], 1)

    total <- rep(NA_real_, ns + 1)
    names(total) <- c(segments, "total")
    for (i in seq_len(ns))
        total[[i]] <- sum(df[[i + 1]] * later_r)
    total[[ns + 1]] <- cumprod(df[["total"]] + 1)[[nt]] - 1
    list(period_contributions = df,
         total_contributions = total)
}


## TODO move to dev
##
## if (FALSE) {

##     prices <- c(100 ,102 ,104 ,104 ,104.5 ,
##                 2   ,2.2 ,2.4 ,2.3 ,2.5   ,
##                 3.5 ,3   ,3.1 ,3.2 ,3.1)
    
##     dim(prices) <- c(5,3)
    
##     weights <- c(1,0,0)
##     when <- as.logical(c(1,0,1,0,1))
##     when <- as.logical(c(1,1,1,1,1))
##     when <- TRUE
    
    
    
##     ## TESTS
##     weights <- c(1,0,0)
##     all.equal(c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0)),
##               c(returns(prices[,1],pad=0)))
##     weights <- c(0,1,0)
##     all.equal(c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0)),
##               c(returns(prices[,2],pad=0)))
##     weights <- c(0,0,1)
##     all.equal(c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0)),
##               c(returns(prices[,3],pad=0)))
##     weights <- c(.5,.4,.1)
##     all.equal(c(returns(prices,pad=0) %*% weights),
##               c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0)))
    
##     when <- as.logical(c(1,0,0,0,0))
##     !isTRUE(all.equal(
##          c(returns(prices,pad=0) %*% weights),
##          c(returns.rebalance(prices, weights, when = when, aggregate = TRUE, pad = 0))))
    
    
    
##     ## not exported
##     prices <- c(100 ,102 ,104 ,104 ,104.5 ,
##                 2   ,2.2 ,2.4 ,2.3 ,2.5   ,
##                 3.5 ,3   ,3.1 ,3.2 ,3.1)
    
##     dim(prices) <- c(5,3)
    
##     weights <- c(1,0,0)
##     when <- as.logical(c(1,1,1,1,1))
##     when <- as.logical(c(1,0,1,0,1))
    
##     returns_rebalance2 <- function(prices, weights, when = NULL, pad = NULL) {
##         prices <- as.matrix(prices)
        
##         nr <- nrow(prices)
##         pos <- array(NA, dim = dim(prices))
##         ## w <- array(NA, dim = dim(prices))
        
##         if (is.null(when && identical(when, TRUE)))
##             when <- seq_len(nc)
##         else if (is.logical(when))
##             when <- which(when)
    
    
##         for (i in when)
##             pos[i, ] <- weights / prices[i, ] ## TODO: if w is matrix, w[i, ]
##         ## TODO na.locf
        
##         pos <- na.locf(pos)
##         pos[is.na(pos)] <- 0
##         w <- pos * prices
##         w <- w/rowSums(w)
        
##         rowSums(returns(prices) * w[-nr,])
##     }
    
##     returns_rebalance2(prices, weights, when = TRUE)
##     PMwR:::returns_rebalance(prices, weights, when = TRUE)
    
##     require("rbenchmark")
##     benchmark(returns_rebalance2(prices, weights, when = TRUE),
##               PMwR:::returns_rebalance(prices, weights, when = TRUE),
##               columns = c("test", "relative", "elapsed"), replications = 1000)

    
## }

as.matrix.p_returns <- function(x, ...) {
    
    if (attr(x, "period") == "monthly" && is.null(dim(x))) {
        t <- attr(x, "t")
        x <- unclass(x)
        
        years <- as.numeric(format(t, "%Y"))
        mons  <- as.numeric(format(t, "%m"))
        tb <- array(NA, dim = c(length(unique(years)), 13L))
        tb[cbind(years - years[1L] + 1L, mons)] <- x
        for (y in suy <- sort(unique(years)))
            tb[y - years[1L] + 1L, 13L] <- prod(x[years==y & !is.na(x)] + 1) - 1
        rownames(tb) <- suy
        colnames(tb) <- c(1:12, "YTD")
        tb
    } else {
        res <- unclass(x)
        res <- as.matrix(res)
        if (!is.null(attr(x, "t")))
            rownames(res) <- as.character(attr(x, "t"))
        attr(res, "t") <- NULL
        attr(res, "period") <- NULL
        res
    }

}

as.data.frame.p_returns <- function(x, ...)
    as.data.frame(as.matrix(x))
