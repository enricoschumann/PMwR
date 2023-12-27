## -*- truncate-lines: t; -*-
## Copyright (C) 2008-23  Enrico Schumann

returns <- function(x, ...)
    UseMethod("returns")

returns.default <-
function(x,
         t = NULL,
         period = NULL,
         complete.first = TRUE,
         pad = NULL,
         position = NULL,
         weights = NULL,
         rebalance.when = NULL,
         lag = 1,
         na.rm = TRUE,
         ...) {

    if (identical(tolower(period), "total"))
        period <- "itd"

    if (is.unsorted(t)) {  ## is.unsorted(NULL) == FALSE
        idx <- order(t)
        t <- t[idx]
        x <- if (is.null(dim(x)))
                 x[idx]
             else
                 x[idx, ]
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
    } else if (is.logical(rebalance.when)) {
        if (isTRUE(rebalance.when)) {
            rebalance.when <- seq_len(nrow(x))
        } else if (!all(rebalance.when)) {         ## only FALSE values
            ## TODO add argument FALSE.NA == FALSE ?
            ##      if TRUE, make NA?
            ans <- rep.int(0, nrow(x))
            if (!is.null(pad))
                ans[1L] <- pad
            else
                ans <- ans[-1L]
            return(ans)
        } else
            rebalance.when <- which(rebalance.when)
    }

    if (!is.null(t) &&
        !is.null(rebalance.when)) {

        if ( is.integer(t) && !is.integer(rebalance.when) &&
            all(as.integer(rebalance.when) == rebalance.when)) {
            rebalance.when <- as.integer(rebalance.when)
        }

        if (!is.integer(t) &&  is.integer(rebalance.when) &&
            all(as.numeric(rebalance.when) == rebalance.when)) {
            rebalance.when <- as.numeric(rebalance.when)
        }
    }

    if (!is.null(t) &&
        inherits(rebalance.when, class(t))) {
        ## TODO: support match_or_next/previous?
        ii <- match(rebalance.when, t)
        if (any(is.na(ii)))
            warning(sQuote("rebalance.when") ,
                    " does not match timestamp")
        rebalance.when <- ii[!is.na(ii)] ## TODO: really drop?
        ## TODO: if weights/position is a matrix
    }

    if (is.null(t) &&
        is.character(period) && period != "itd") {
        warning("no timestamp information available, so ",
                sQuote("period"), " is ignored")
        period <- NULL
    }

    if (is.null(period) && is.null(position) && is.null(weights)) {
        .returns(x, pad = pad, lag = lag)
    } else if (is.null(position) && !is.null(weights)) {
        if (lag != 1L)
            warning(sQuote("lag"), " is ignored")
        x <- as.matrix(x)
        returns_position(prices = x, positions = weights,
                         when = rebalance.when, pad = pad,
                         weights = TRUE)
    } else if (!is.null(period)) {
        if (lag != 1L)
            warning(sQuote("lag"), " is ignored")
        pReturns(x, t, period, complete.first, pad = pad, na.rm = na.rm)
    } else {
        if (lag != 1L)
            warning(sQuote("lag"), " is ignored")
        x <- as.matrix(x)
        returns_position(prices = x, positions = position,
                         when = rebalance.when, pad = pad,
                         weights = FALSE)
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
                              pad = NULL, position = NULL, lag = 1,
                              na.rm = TRUE, ...) {

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
                        lag = 1, na.rm = TRUE, ...) {

    t <- time(x)
    x <- coredata(x)

    if (!is.null(period)) {
        returns.default(x, t = t, period = period,
                        complete.first = complete.first,
                        pad = pad, position = position,
                        weights = weights,
                        rebalance.when = rebalance.when,
                        lag = lag, na.rm = na.rm, ...)
    } else {
        ans <-
        returns.default(x, t = t, period = NULL,
                        complete.first = complete.first,
                        pad = pad, position = position,
                        weights = weights,
                        rebalance.when = rebalance.when,
                        lag = lag,  na.rm = na.rm, ...)
        attrs <- attributes(ans)
        ans <- if (!is.null(pad))
                   zoo(ans, t)
               else
                   zoo(ans, t[-seq_len(lag)])
        attr(ans, "holdings") <- attrs$holdings
        attr(ans, "contributions") <- attrs$contributions
        ans
    }
}

returns.data.frame <- function(x, t = NULL, period = NULL,
                               complete.first = TRUE,
                               pad = NULL, position = NULL,
                               weights = NULL, rebalance.when = NULL,
                               lag = 1, na.rm = TRUE, ...) {

    ans <- returns.default(x, t = t, period = period,
                           complete.first = complete.first,
                           pad = pad, position = position,
                           weights = weights,
                           rebalance.when = rebalance.when,
                           lag = lag, ...)
    ans
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
        rets <- x[-a, , drop = FALSE] / x[-b, , drop = FALSE] - 1
        if (do.pad)
            rets <- do.call("rbind",
                            c(as.list(rep(pad, lag)), list(rets)))
    }
    rets
}



## not exported
pReturns <- function(x, t, period, complete.first = TRUE,
                     pad = NULL, na.rm = na.rm) {
    ## TODO add also: 'previous month' and pattern 'YYYY-MM'?
    ## TODO add also: 'ytm' ("year to month")
    ## TODO add also: 'irr' (NMOF::ytm)

    x <- as.matrix(x)
    if (!is.null(colnames(x)))
        instr <- colnames(x)
    else
        instr <- NULL
    nc <- ncol(x)
    if (is.character(period))
        period <- tolower(period)


    ## TODO have periods such as "best 5 years"?
    if (length(period) == 1L &&
        (best <- grepl("best",  period, ignore.case = TRUE) ||
                 grepl("worst", period, ignore.case = TRUE))) {

        if (grepl("year", period, ignore.case = TRUE)) {

        } else if (grepl("quarter", period, ignore.case = TRUE)) {

        } else if (grepl("month",   period, ignore.case = TRUE)) {

        } else if (grepl("day",     period, ignore.case = TRUE)) {

        } else if (grepl("hour",    period, ignore.case = TRUE)) {

        }

    } else if (length(period) == 1L &&
               grepl("^ann", period, ignore.case = TRUE)) {
        if (!is.null(pad))
            warning(sQuote("pad"), " is ignored")
        force <- grepl("!$", period)
        ans <- numeric(nc)
        names(ans) <- instr
        is.ann  <- logical(nc)
        from.to <- vector("list", length = nc)

        if (inherits(t, "yearmon")) {
            units_per_year <- 1
        } else if (inherits(t, "yearqtr")) {
            units_per_year <- 1
        } else {
            units_per_year <- 365
            t <- as.Date(t)
        }

        t0 <- 1
        t1 <- nrow(x)
        for (j in 1:nc) {
            xj <- x[, j]
            if (na.rm) {
                tmp <- which(!is.na(xj))
                t0 <- min(tmp)
                t1 <- max(tmp)
            }
            tt <- as.numeric( t[t1] - t[t0] )/units_per_year
            tmp <- xj[t1] / xj[t0]
            if (tt > 1 || force) {
                tmp <- tmp^(1/tt)
                is.ann[j] <- TRUE
            }
            ans[j] <- tmp - 1
            from.to[[j]] <- c(t[t0], t[t1])
        }
        attr(ans, "period") <-
            if (force) "annualised!" else "annualised"

        ## --
        from.to <- do.call(rbind, from.to)
        colnames(from.to) <- c("from", "to")
        if (inherits(t, "yearmon")) {
            class(from.to) <- "yearmon"
        } else if (inherits(t, "yearqtr")) {
            class(from.to) <- "yearqtr"
        } else {
            class(from.to) <- "Date"
        }
        attr(ans, "t") <- from.to
        ## --

        attr(ans, "is.annualised") <- is.ann
    } else if (length(period) == 1L && period == "itd") {
        if (!is.null(pad))
            warning(sQuote("pad"), " is ignored")
        ans <- numeric(nc)
        names(ans) <- instr
        from.to <- vector("list", length = nc)
        for (j in 1:nc) {
            xj <- x[, j]
            na <- which(!is.na(xj))
            if (length(na) <= 1) {
                ans[j] <- NA
            } else {
                t0 <- min(na)
                t1 <- max(na)
                ans[j] <- drop(.returns(xj[c(t0, t1)], lag = 1))
                if (!is.null(t))
                    from.to[[j]] <- c(t[t0], t[t1])
            }
        }

        ## --
        if (!is.null(unlist(from.to)) && length(from.to)) {
            from.to <- do.call(rbind, from.to)
            colnames(from.to) <- c("from", "to")
            if (inherits(t, "yearmon")) {
                class(from.to) <- "yearmon"
            } else if (inherits(t, "yearqtr")) {
                class(from.to) <- "yearqtr"
            } else {
                class(from.to) <- "Date"
            }
        }
        ## --

        attr(ans, "t") <- from.to
        attr(ans, "period") <- "itd"
    } else if (length(period) == 1L &&
               grepl("^ytd", period, ignore.case = TRUE)) {
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
        from.to <- vector("list", length = nc)
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
            from.to[[j]] <- c(t[t0], t[t1])
        }

        ## --
        from.to <- do.call(rbind, from.to)
        colnames(from.to) <- c("from", "to")
        if (inherits(t, "yearmon")) {
            class(from.to) <- "yearmon"
        } else if (inherits(t, "yearqtr")) {
            class(from.to) <- "yearqtr"
        } else {
            class(from.to) <- "Date"
        }
        attr(ans, "t") <- from.to
        ## --

        attr(ans, "period") <- "ytd"
    } else if (length(period) == 1L &&
               grepl("^ytm", period, ignore.case = TRUE)) {
        ## compute returns up to most recent month-end
        if (!is.null(pad))
            warning(sQuote("pad"), " is ignored")
        ymon <- as.numeric(format(t, "%Y%m"))
        years <- as.numeric(format(t, "%Y"))
        if (period != "ytm!" && max(years) != as.numeric(format(Sys.Date(), "%Y")))
            warning("max. timestamp (", max(years), ") does not match current year")
        ans <- numeric(nc)
        from.to <- vector("list", length = nc)
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
            from.to[[j]] <- c(t[t0], t[t1])
        }

        ## --
        from.to <- do.call(rbind, from.to)
        colnames(from.to) <- c("from", "to")
        if (inherits(t, "yearmon")) {
            class(from.to) <- "yearmon"
        } else if (inherits(t, "yearqtr")) {
            class(from.to) <- "yearqtr"
        } else {
            class(from.to) <- "Date"
        }
        attr(ans, "t") <- from.to
        ## --

        attr(ans, "period") <- "ytd"

    } else if (length(period) == 1L &&
               period == "mtd") {
        if (inherits(t, "yearqtr") ||
            inherits(t, "yearmon")) {
            warning("period ", sQuote("mtd"), " make no sense for yearmon/-qtr")
        }
        if (!is.null(pad))
            warning(sQuote("pad"), " is ignored")
        ymon <- as.numeric(format(t, "%Y%m"))
        ans <- numeric(nc)
        names(ans) <- instr

        from.to <- vector("list", length = nc)
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
            from.to[[j]] <- c(t[t0], t[t1])
        }

        ## --
        from.to <- do.call(rbind, from.to)
        colnames(from.to) <- c("from", "to")
        if (inherits(t, "yearmon")) {
            class(from.to) <- "yearmon"
        } else if (inherits(t, "yearqtr")) {
            class(from.to) <- "yearqtr"
        } else {
            class(from.to) <- "Date"
        }
        attr(ans, "t") <- from.to
        ## --

        attr(ans, "period") <- "mtd"

    } else if (length(period) == 1L &&
               (grepl("[0-9][0-9][0-9][0-9]", period, ignore.case = TRUE))) {
        m <- which(format(t, "%Y") == period)
        ii <- c(max(1, min(m)-1), max(m))
        ans <- returns(x[ii, ], pad = pad)
        attr(ans, "t") <- t(t[ii])
        class(attr(ans, "t")) <- "Date"
        attr(ans, "period") <- period

    } else if (length(period) > 1L &&
               length(period) < NROW(x)) {
        ans <- returns(x[period, ], pad = pad)
        if (!is.null(t))
            attr(ans, "t") <- t(t[period])
        attr(ans, "period") <- period

    } else {
        if (length(period) == NROW(x)) {
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
    if (identical(period, "monthly"))
        class(ans) <- c("p_returns_monthly", class(ans))

    ans
}

as.zoo.p_returns <- function(x, ...) {
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

    if (inherits(timestamp, "yearmon")) {
        units_per_year <- 1
    } else if (inherits(timestamp, "yearqtr")) {
        units_per_year <- 1
    } else {
        units_per_year <- 365
    }

    if (identical(period, "yearly") && is.null(dim(x))) {
        tmp <- x
        names(tmp) <- format(timestamp, "%Y")
        if (year.rows)
            print(fmt(tmp, plus, digits), quote = FALSE)
        else
            print(as.matrix(fmt(tmp, plus, digits)), quote = FALSE)
    } else if (identical(period, "yearly")) {
        tmp <- unclass(x)
        rownames(tmp) <- format(timestamp, "%Y")
        attr(tmp, "t") <- NULL
        attr(tmp, "period") <- NULL
        if (year.rows) {
            print(fmt(tmp, plus, digits), quote = FALSE, print.gap = 1, right = TRUE)
        } else {
            print(fmt(t(tmp), plus, digits), quote = FALSE, print.gap = 1, right = TRUE)
        }
    } else if (identical(period, "annualised")) {
        if (!is.null(instr))
            nn <- paste0(format(instr,
                         width = max(nchar(instr)),
                         justify = "right"), ": ")
        else
            nn <- ""
        r_str <- paste0(format(round(x*100, digits), nsmall = digits), "%  ")
        if (inherits(timestamp, "Date")) {
            cal_str <- paste0("[",
                              format(timestamp[, 1],"%d %b %Y"), " -- ",
                              format(timestamp[, 2],"%d %b %Y"))
        } else if (inherits(timestamp, "yearmon")) {
            cal_str <- paste0("[",
                              format(timestamp[, 1],"%b %Y"), " -- ",
                              format(timestamp[, 2],"%b %Y"))
        } else if (inherits(timestamp, "yearqtr")) {
            cal_str <- paste0("[",
                              format(timestamp[, 1],"Q%q %Y"), " -- ",
                              format(timestamp[, 2],"Q%q %Y"))
        }

        note <- rep("]", length(x))
        note[as.numeric(timestamp[, 2L] - timestamp[, 1L])/units_per_year < 1 &
              attr(x, "is.annualised")] <-
            "; less than one year, but annualised]"
        note[as.numeric(timestamp[, 2L] - timestamp[, 1L])/units_per_year < 1 &
              !attr(x, "is.annualised")] <-
            "; less than one year, not annualised]"
        cat(paste0(nn, r_str, cal_str, note, collapse = "\n"), "\n")
    } else if (length(period) == 1 &&
               (period == "ytd" || period == "mtd" || period == "itd" ||
                grepl("^[0-9][0-9][0-9][0-9]$", period))) {
        if (!is.null(instr))
            nn <- paste0(format(instr,
                         width = max(nchar(instr)),
                         justify = "right"), ": ")
        else
            nn <- ""
        r_str <- paste0(format(round(x*100, digits), nsmall = digits), "%  ")

        if (inherits(timestamp, "Date")) {
            cal_str <- paste0("[",
                              format(timestamp[, 1],"%d %b %Y"), " -- ",
                              format(timestamp[, 2],"%d %b %Y"))
        } else if (inherits(timestamp, "yearmon")) {
            cal_str <- paste0("[",
                              format(timestamp[, 1],"%b %Y"), " -- ",
                              format(timestamp[, 2],"%b %Y"))
        } else if (inherits(timestamp, "yearqtr")) {
            cal_str <- paste0("[",
                              format(timestamp[, 1],"Q%q %Y"), " -- ",
                              format(timestamp[, 2],"Q%q %Y"))
        }
        cat(paste0(nn, r_str, cal_str, "]", collapse = "\n"), "\n")
    } else {
        print(unclass(x))
    }
    invisible(x)
}

## not exported
toLatex.p_returns <- function(object, ..., year.rows = TRUE,
                              ytd = "YTD", month.names = NULL,
                              eol = "\\\\",
                              stand.alone = FALSE) {

    stop("currently only supported for period ", sQuote("month"))
}

toLatex.p_returns_monthly <- function(object, ...,
                                      year.rows = TRUE,
                                      ytd = "YTD", month.names = NULL,
                                      eol = "\\\\",
                                      include.colnames = TRUE,
                                      include.rownames = TRUE,
                                      stand.alone = FALSE) {

    period <- attr(object, "period")
    timestamp <- attr(object, "t")

    if (year.rows)
        mt <-   .mtab(object, timestamp, ytd = ytd, month.names = month.names, ...)
    else
        mt <- t(.mtab(object, timestamp, ytd = ytd, month.names = month.names, ...))

    if (include.colnames)
        mt <- rbind(colnames(mt), mt)
    if (include.rownames)
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

    .th <- function(x, style = th.style, class = th.class) {
        open <- paste0("<th", .ctag(style, "style"), .ctag(class, "class"), ">")
        paste0(open, x, "</th>")
    }
    .td <- function(x, style = td.style, class = td.class) {
        open <- paste0("<td", .ctag(style, "style"), .ctag(class, "class"), ">")
        paste0(open, x, "</td>")
    }
    .tr <- function(x, style = tr.style, class = tr.class) {
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
    mt <- apply(mt, 2,
                function(x) {
                    format(x,
                           width = max(5, nchar(x)),
                           justify = "right")
    })
    mt <- apply(mt, 1, paste0, collapse = "")
    class(mt) <- "text"
    mt
}

## not exported
returns_rebalance <- function(prices, weights,
                              when = NULL, pad = NULL) {

    prices <- as.matrix(prices)
    nr <- nrow(prices)
    nc <- ncol(prices)
    if (nr < 2L)
        stop("fewer than two price observations")

    if (is.null(dim(weights)) && is.null(when)) {
        ## TODO faster implementation?
    }

    ## when: must become a logical vector of
    ##       length 'nr'
    if (is.null(when) || isTRUE(when))
        when <- rep(TRUE, nr)
    else if (identical(when, FALSE))
        when <- rep(FALSE, nr)
    else if (is.numeric(when)) {
        ## cases such as '1, 4, 12, 23, ...'
        when <- unique(when)
        tmp <- logical(nr)
        tmp[round(when)] <- TRUE
        when <- tmp
    }

    ## weights/prices:
    ## do not touch weights if it has as many rows as prices

    if (nc == 1L) {
        ## prices == single column.
        ## weights is made into a column vector,
        ## irrespective of shape (no dim, row,
        ## or column)
        dim(weights) <- c(length(weights), 1L)
    } else {
        ## prices has more than one column.
        ## if weights has as many elements as assets (nc),
        ## make it a row vector, irrespective of shape
        ## (no dim, row, or column)
        if (length(weights) == nc) {
            dim(weights) <- c(1L, nc)
            tmp <- rep(weights, each = sum(when))
            dim(tmp) <- c(sum(when), nc)
            weights <- tmp
        }
    }

    if (dim(weights)[2L] != nc)
        stop("weights do not match number of price series")

    if (nrow(weights) < nr) {
        tmp <- array(0, dim = c(nr, nc))
        ## if (when[1])
        ##     tmp[1L, ] <- weights[1L, ]
        if (sum(when) != nrow(weights))
            warning("rebalance.when does not match nrow(weights)")

        when.n <- which(when)
        j <- 0
        for (i in 1L:nr) {
            if (when[i]) {
                j <- j+1
                tmp[i, ] <- weights[j, ]
            } else if (i > 1)
                tmp[i, ] <- tmp[i-1, ]
        }
        weights <- tmp
    }

    val <- numeric(nr) + 1
    h <- ctb <- array(0, dim = dim(prices))

    if (any(when)) {
        first <- which(when)[1L]
        val[seq(to = first)] <- 1
        cash <- 1 - sum(weights[first, ])
        h[first, ] <- weights[first, ]/prices[first, ]

        if (first < nr)
            for (i in (first+1):nr) {
                val[i] <- sum(prices[i, ] * h[i-1, ]) + cash
                ctb[i, ] <- (prices[i, ]-prices[i-1, ]) *
                    h[i-1, ] / val[i-1]
                if (when[i]) {
                    h[i, ] <- val[i] * weights[i, ]/prices[i, ]
                    cash <- val[i] - sum(weights[i, ])*val[i]
                } else
                    h[i, ] <- h[i - 1, ]
            }
    }
    ans <- .returns(val, pad = pad, lag = 1L)
    attr(ans, "holdings") <- h
    attr(ans, "contributions") <- ctb
    ans
}

t.p_returns <- function(x)
    t(as.matrix.p_returns(x))

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

## not exported
returns_position <- function(prices,
                             positions,
                             when = NULL,
                             pad = NULL,
                             weights = FALSE, ...) {

    if (is.null(when))
        when <- seq_len(nrow(prices))

    if (anyDuplicated(when))
        warning("duplicated values in ", sQuote("when"))

    if (is.null(dim(prices)))
        dim(prices) <- c(length(prices), 1L)

    if (is.null(dim(positions))) {
        ## TODO handle special cases: one price, vector
        ##      of weights
        if (ncol(prices) == 1L)
            dim(positions) <- c(length(positions), 1L)
        else
            dim(positions) <- c(1L, length(positions))
    }

    if (length(when) > 1L && nrow(positions) == 1L) {
        n <- length(when)
        tmp <- rep(positions, each = n)
        dim(tmp) <- c(n, ncol(positions))
        positions <- tmp
    }

    if (nrow(positions) == nrow(prices))
        positions <- positions[when, , drop = FALSE]

    i <- match(nrow(prices), when, nomatch = 0L)
    if (i > 0L) {
        when <- when[-i]
        positions <- positions[-i, ]
    }

    if (any(is.na(prices[when, ])))
        warning("missing values at rebalance times")

    if (!length(when)) {
        return(rep.int(0, nrow(prices))) ## FIXME: pad ignored?
    }

    if (weights) {
        cash <- 1 - rowSums(positions)
        if (any(abs(cash) > sqrt(.Machine$double.eps))) {
            prices <- cbind(prices, 1)
            positions <- cbind(positions, cash)
        }
        positions <- positions/prices[when, ]
    }

    X <- array(NA_real_, dim = dim(prices))
    X[when, ] <- positions
    X <- .copy_fw_matrix(X)
    if (when[1L] > 1L)
        X[seq_len(when[1L] - 1L), ] <- 0

    zero.pos <- apply(X, 1, function(x) all(abs(x) < sqrt(.Machine$double.eps)))
    contrib <- diff(prices)*X[-nrow(X), ]/rowSums(X*prices)[-nrow(X)]
    contrib[zero.pos[-length(zero.pos)], ] <- 0
    if (!is.null(pad)) {
        contrib <- rbind(pad, contrib)
        contrib[seq_len(when[1L]), ] <- pad
    } else {
        if (when[1L] > 1L)
            contrib <- contrib[-seq_len(when[1L]-1L), ]
    }
    ans <- unname(rowSums(contrib))
    attr(ans, "holdings") <- X
    attr(ans, "contributions") <- contrib
    ans

}
