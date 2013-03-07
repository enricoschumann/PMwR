previousBusinessDay <- function (x, holidays = NULL) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    x <- x - 1
    tmp <- as.POSIXlt(x)
    tmpi <- tmp$wday == 6L
    x[tmpi] <- x[tmpi] - 1L
    tmpi <- tmp$wday == 0L
    x[tmpi] <- x[tmpi] - 2L
    x
}
nextBusinessDay <- function(x, holidays = NULL) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    x <- x + 1
    tmp <- as.POSIXlt(x)
    tmpi <- tmp$wday == 6L
    x[tmpi] <- x[tmpi] + 2L
    tmpi <- tmp$wday == 0L
    x[tmpi] <- x[tmpi] + 1L
    x
}
isWeekend <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$wday == 0L | tmp$wday == 6L
}
firstOfMonth <- function (x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    as.Date(tmp)
}
endOfMonth <- function(x, shift = 0L) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mon <- tmp$mon + 1 + shift
    tmp$mday <- 1L
    as.Date(tmp) - 1
}
endOfPreviousMonth <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    as.Date(tmp) - 1
}
dayOfMonth <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    as.POSIXlt(x)$mday
}
`dayOfMonth<-` <- function(x, value){
    if (!all(inherits(x, "Date") | inherits(x, "POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    cl <- class(x)
    tmp <- as.POSIXlt(x)
    tmp$mday <- value
    if (cl == "Date")
        as.Date(tmp)
    else if (cl == "POSIXct")
        as.POSIXct(tmp)
    else 
        tmp
}
    
timegrid <- function(from, to, interval,
                     excludeWeekends = TRUE,
                     holidays   = NULL,
                     fromHHMMSS = "080000",
                     toHHMMSS   = "220000") {

    fromHHMMSS <- makeHHMMSS(fromHHMMSS)
    toHHMMSS   <- makeHHMMSS(toHHMMSS)

    if (!inherits(from, "POSIXt") || !inherits(to, "POSIXt"))
        stop("'from' and 'to' must inherit from 'POSIXt'")
    grd <- seq(from, to, by = interval)
    if (!is.null(holidays)) {
        if (!inherits(holidays, "Date")) {
            as.Date(holidays)
        }
        grd <- grd[!(as.Date(grd) %in% holidays)]
    }
    grd <- as.POSIXlt(grd)
    grd <- grd[grd$wday > 0L & grd$wday < 6L] ## remove weekends
    tmp <- paste(sprintf("%02d",grd$hour),
                 sprintf("%02d",grd$min),
                 sprintf("%02d",grd$sec), sep = "")
    grd <- grd[fromHHMMSS <= tmp & toHHMMSS >= tmp]
    as.POSIXct(grd)
}
roundPOSIXt <- function(t, interval) {

    if (!inherits(t, "POSIXct"))
        t <- as.POSIXct(t)
    tz <- attr(t, "tzone")
    n <- as.integer(strsplit(interval, " ", fixed = TRUE)[[c(1L,1L)]])

    if (grepl("sec", interval, ignore.case = TRUE)) {
        ## Default unit in 'interval' is 'sec': do nothing.
    } else if (grepl("min", interval, ignore.case = TRUE)) {
        n <- 60L * n
    } else if (grepl("hour", interval, ignore.case = TRUE)) {
        n <- 60L * 60L * n
    } else if (grepl("day", interval, ignore.case = TRUE)) {
        n <- 24L * 60L * 60L * n
    } else {
        stop("'interval' not clear")
    }
    .POSIXct(n*floor(unclass(t)/n), tz = tz)
}
plotTradingHours <- function(x, t = NULL,
                             interval = "5 min", labels = "hours",
                             excludeWeekends = TRUE, holidays = NULL,
                             fromHHMMSS = "000000", toHHMMSS = "240000",
                             do.plotAxis = TRUE,
                             ...,
                             from = NULL, to = NULL,
                             do.plot = TRUE, do.lines = FALSE,
                             ppar = list(),
                             axis1.par = list(),
                             axis2.par = list(),
                             ablineh.par = list(),
                             ablinev.par = list()) {

    ## par
    ppar.def <- list(tck = 0.001, mgp = c(3,1,0.5), bty = "n")
    ppar.def[names(ppar)] <- ppar
    ppar <- ppar.def

    ## plot
    plot.par.def <- list(type = "l", xaxt = "n", yaxt = "n",
                         xlab = "", ylab = "")
    tmp <- list(...)
    plot.par.def[names(tmp)] <- tmp
    plot.par <- plot.par.def

    ## lines
    lines.par.def <- list(type = "l", xlab = "", ylab = "")
    tmp <- list(...)
    lines.par.def[names(tmp)] <- tmp
    lines.par <- lines.par.def

    ## axis1
    axis1.par.def <- list(1, at = quote(pos),
                          labels = quote(format(grd[pos], fmt)), lwd = 0,
                          col = grey(.5), col.axis=grey(.5))
    axis1.par.def[names(axis1.par)] <- axis1.par
    axis1.par <- axis1.par.def

    ## axis2
    axis2.par.def <- list(2, at = quote(axTicks(2L)),
                          labels = quote(round(axTicks(2L),3)),
                          las = 2, lwd = 0, col = grey(0.5),
                          col.axis = grey(0.5))
    axis2.par.def[names(axis2.par)] <- axis2.par
    axis2.par <- axis2.par.def

    ## ablineh.par
    if (length(ablineh.par)==0L || !is.na(ablineh.par)) {
        ablineh.par.def <- list(h = quote(axTicks(2)), lty=3, col=grey(0.5))
        ablineh.par.def[names(ablineh.par)] <- ablineh.par
        ablineh.par <- ablineh.par.def
    }

    ## ablinev.par
    if (length(ablinev.par)==0L || !is.na(ablinev.par)) {
        ablinev.par.def <- list(v = quote(pos), col = grey(0.5), lty = 3)
        ablinev.par.def[names(ablinev.par)] <- ablinev.par
        ablinev.par <- ablinev.par.def
    }

    if (is.null(t)) {
        if (!inherits(x, "zoo"))
            stop(sQuote("t"), " not supplied, so ", sQuote("x"), " must inherit from ",
                 sQuote("zoo"))
        else {
            t <- index(x)
            x <- coredata(x)
        }
    } else {
        if (is.unsorted(t)) {
            idx <- order(t)
            x <- x[idx]
            t <- t[idx]
        }
    }


    ## check/prepare times
    fromHHMMSS <- makeHHMMSS(fromHHMMSS)
    toHHMMSS   <- makeHHMMSS(toHHMMSS)

    getLast <- function(x, by) {
        lby <- length(by)
        rby <- by[lby:1L]
        x[lby - match(unique(by), rby) + 1L]
    }

    if (is.null(from))
        from <- roundPOSIXt(t[1L], interval)
    if (is.null(to))
        to <- roundPOSIXt(t[length(t)], interval)

    grd <- timegrid(from, to, interval = interval,
                    fromHHMMSS = fromHHMMSS, toHHMMSS = toHHMMSS)

    ## aggregate data to grid (last)
    by <- roundPOSIXt(t, interval = interval)
    values <- getLast(x, by)
    t <- unique(by)

    ## match to grid
    ri <- match(grd, t, nomatch = 0L)
    rx <- match(t[ri], grd)

    ## prepare labels
    if (grepl("hour", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.POSIXlt(grd)$hour)) > 0) + 1
        fmt <- "%H:%M"
    } else if (grepl("dayhour", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.POSIXlt(grd)$hour)) > 0) + 1
        fmt <- "%d.%m. %H:%M"
    } else if (grepl("day", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.Date(grd))) > 0) + 1
        fmt <- "%d.%m."
    } else if (grepl("month", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.POSIXlt(grd)$mon)) > 0) + 1
        fmt <- "%d.%m."
    }

    if (do.lines) {
        do.call("lines", c(list(x = seq_len(length(grd))[rx],
                                y = values[ri]),
                           lines.par))
        NULL
    } else if (do.plot) {
        do.call("par", ppar)
        do.call("plot", c(list(x = seq_len(length(grd))[rx],
                               y = values[ri]),
                          plot.par))

        if (length(axis2.par) && !is.na(axis2.par))
            do.call("axis", axis2.par)
        if (length(ablineh.par) && !is.na(ablineh.par))
            do.call("abline", ablineh.par)
        if (do.plotAxis && length(axis1.par) && !is.na(axis1.par))
            do.call("axis", axis1.par)
        if (length(ablinev.par) && !is.na(ablinev.par))
            do.call("abline", ablinev.par)
        list(t = seq_len(length(grd))[rx],
             x = values[ri],
             axis.pos = pos,
             axis.labels = format(grd[pos], fmt),
             timegrid = grd)
    } else {
        list(t = seq_len(length(grd))[rx],
             x = values[ri],
             axis.pos = pos,
             axis.labels = format(grd[pos], fmt),
             timegrid = grd)
    }
}

lastWeekday <- function(weekday, date, shift = 0L,
                        period = "month", before, inclusive = TRUE) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mon <- tmp$mon + 1L
    tmp$mday <- 1L
    ldate <- as.Date(tmp) - 1L
    lweekday <- as.POSIXlt(ldate)$wday
    ldate - (lweekday - weekday)%%7L - (shift*7L)
}
