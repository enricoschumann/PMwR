## -*- truncate-lines: t; -*-
## Copyright (C) 2008-19  Enrico Schumann

plot_trading_hours <-
    plotTradingHours <- function(x, t = NULL,
             interval = "5 min", labels = "hours",
             label.format = NULL,
             exclude.weekends = TRUE, holidays = NULL,
             fromHHMMSS = "000000", toHHMMSS = "240000",
             do.plot.axis = TRUE,
             ...,
             from = NULL, to = NULL,
             do.plot = TRUE,
             axis1.par = list()) {
    if (as.character(sys.call(sys.parent()))[1L] == "plotTradingHours")
        .Deprecated("plot_trading_hours")

    ## plot
    plot.par.def <- list(type = "l", xaxt = "n",
                         xlab = "", ylab = "")
    tmp <- list(...)
    plot.par.def[names(tmp)] <- tmp
    plot.par <- plot.par.def

    ## axis1
    axis1.par.def <- list(1, at = quote(pos),
                          labels = quote(format(grd[pos], fmt)))
    axis1.par.def[names(axis1.par)] <- axis1.par
    axis1.par <- axis1.par.def

    if (is.null(t)) {
        if (!inherits(x, "zoo"))
            stop(sQuote("t"), " not supplied, so ", sQuote("x"),
                 " must inherit from ", sQuote("zoo"))
        else {
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

    if (inherits(t, "Date")) {
        if (!labels %in% c("day", "month"))
            labels <- "day"
        interval <- "1 day"
    }
    
    fromHHMMSS <- makeHHMMSS(fromHHMMSS)
    toHHMMSS   <- makeHHMMSS(toHHMMSS)

    if (is.null(from))
        from <- roundPOSIXt(t[1L], interval)
    if (is.null(to))
        to <- roundPOSIXt(t[length(t)], interval)

    grd <- timegrid(from, to, interval = interval,
                    holidays = holidays,
                    fromHHMMSS = fromHHMMSS, toHHMMSS = toHHMMSS)
    
    ## aggregate data to grid (last)
    by <- roundPOSIXt(t, interval = interval)
    values <- last(x, by)
    if (inherits(t, "Date")) {
        by <- as.Date(as.POSIXlt(by))
        grd <- as.Date(as.POSIXlt(grd))
    }
    t <- unique(by)
    
    ## match to grid
    ri <- match(grd, t, nomatch = 0L)
    rx <- match(t[ri], grd)
    values <- values[ri]    

    maptime <- function(t) {
        ## interval, grd in environment
        by <- roundPOSIXt(t, interval = interval)
        ri <- match(t, grd, nomatch = NA)
        list(t = ri[!is.na(ri)], ix = which(!is.na(ri)))
    }
    
    ## prepare labels
    if (grepl("dayhour", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.POSIXlt(grd)$hour)) > 0) + 1
        fmt <- if (is.null(label.format))
                   "%d.%m. %H:%M" else label.format
    } else if (grepl("hour", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.POSIXlt(grd)$hour)) > 0) + 1
        fmt <- if (is.null(label.format))
                   "%H:%M" else label.format
    } else if (grepl("day", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.Date(as.POSIXlt(grd)))) > 0) + 1
        fmt <- if (is.null(label.format))
                   "%d.%m." else label.format
    } else if (grepl("month", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.POSIXlt(grd)$mon)) > 0) + 1
        fmt <- if (is.null(label.format))
            "%b %y" else label.format
    }
    
    if (do.plot) {
        do.call("plot", c(list(x = seq_len(length(grd))[rx],
                               y = values),
                          plot.par))

        if (do.plot.axis && length(axis1.par) && !all(is.na(axis1.par)))
            do.call("axis", axis1.par)
        invisible(list(t = seq_len(length(grd))[rx],
                       x = values,
                       axis.pos = pos,
                       axis.labels = format(grd[pos], fmt),
                       timegrid = grd,
                       map = maptime))
    } else {
        list(t = seq_len(length(grd))[rx],
             x = values,
             axis.pos = pos,
             axis.labels = format(grd[pos], fmt),
             timegrid = grd,
             map = maptime)
    }
}
