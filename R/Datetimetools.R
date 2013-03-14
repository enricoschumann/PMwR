
                                        # DATES

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
    x <- x + 1L
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
    tmp$mon <- tmp$mon + 1L + shift
    tmp$mday <- 1L
    as.Date(tmp) - 1L
}
endOfPreviousMonth <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    as.Date(tmp) - 1L
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
lastWeekday <- function(weekday, x, shift = 0L,
                        period = "month", before, inclusive = TRUE) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mon <- tmp$mon + 1L
    tmp$mday <- 1L
    ldate <- as.Date(tmp) - 1L
    lweekday <- as.POSIXlt(ldate)$wday
    ldate - (lweekday - weekday)%%7L + (shift*7L)
}



                                        # TIMES

timegrid <- function(from, to, interval,
                     excludeWeekends = TRUE,
                     holidays   = NULL,
                     fromHHMMSS = "080000",
                     toHHMMSS   = "220000") {

    fromHHMMSS <- makeHHMMSS(fromHHMMSS)
    toHHMMSS   <- makeHHMMSS(toHHMMSS)
    
    if (!inherits(from, "POSIXt") || !inherits(to, "POSIXt"))
        stop(sQuote("from"), " and ", sQuote("to"), " must inherit from POSIXt")
    grd <- seq(from, to, by = interval)
    if (!is.null(holidays)) {
        if (!inherits(holidays, "Date")) {
            as.Date(holidays)
        }
        grd <- grd[!(as.Date(grd) %in% holidays)]
    }
    lt <- as.POSIXlt(grd)
    tmp <- lt$hour*10000 + lt$min*100 + lt$sec
    grd <- grd[lt$wday > 0L & lt$wday < 6L &
               as.numeric(fromHHMMSS) <= tmp & as.numeric(toHHMMSS) >= tmp] 
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
                             label.format = NULL,
                             excludeWeekends = TRUE, holidays = NULL,
                             fromHHMMSS = "000000", toHHMMSS = "240000",
                             do.plotAxis = TRUE,
                             ...,
                             from = NULL, to = NULL,
                             do.plot = TRUE,
                             axis1.par = list()) {

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

    ## check/prepare times
    fromHHMMSS <- makeHHMMSS(fromHHMMSS)
    toHHMMSS   <- makeHHMMSS(toHHMMSS)

    if (is.null(from))
        from <- roundPOSIXt(t[1L], interval)
    if (is.null(to))
        to <- roundPOSIXt(t[length(t)], interval)

    grd <- timegrid(from, to, interval = interval,
                    fromHHMMSS = fromHHMMSS, toHHMMSS = toHHMMSS)
    
    ## aggregate data to grid (last)
    by <- roundPOSIXt(t, interval = interval)
    values <- last(x, by)
    t <- unique(by)
    
    ## match to grid
    ri <- match(grd, t, nomatch = 0L)
    rx <- match(t[ri], grd)
    values <- values[ri]    

    maptime <- function(t) {
        ## interval, grd in environment!
        by <- roundPOSIXt(t, interval = interval)
        t <- unique(by)
        ri <- match(grd, t, nomatch = 0L)
        rx <- match(t[ri], grd)
        list(t = rx, ix = ri[ri > 0])
    }
    
    ## prepare labels
    if (grepl("dayhour", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.POSIXlt(grd)$hour)) > 0) + 1
        fmt <- "%d.%m. %H:%M"
    } else if (grepl("hour", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.POSIXlt(grd)$hour)) > 0) + 1
        fmt <- "%H:%M"
    } else if (grepl("day", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.Date(grd))) > 0) + 1
        fmt <- "%d.%m."
    } else if (grepl("month", labels, ignore.case = TRUE)) {
        pos <- which(abs(diff(as.POSIXlt(grd)$mon)) > 0) + 1
        fmt <- "%d.%m."
    }
    
    if (do.plot) {
        do.call("plot", c(list(x = seq_len(length(grd))[rx],
                               y = values),
                          plot.par))

        if (do.plotAxis && length(axis1.par) && !is.na(axis1.par))
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

