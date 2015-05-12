
                                        # DATES

previousBusinessDay <- function (x, holidays = NULL) {
    if (!all(inherits(x, "Date") | inherits(x, "POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    x <- as.Date(x)
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
    x <- as.Date(x)
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

isLeapyear <- function(x)
    x %% 4 == 0 & (x %% 100 != 0 | x %% 400 == 0)

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

mday <- function(x)
    dayOfMonth(x)

`mday<-` <- function(x, value){
    dayOfMonth(x) <- value
    x
}

dayOfMonth <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    as.POSIXlt(x)$mday
}

`dayOfMonth<-` <- function(x, value) {
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

nthWeekday <- function(weekday, x, n = 1L) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    fweekday <- tmp$wday   
    as.Date(tmp) + (weekday - fweekday) %% 7L + 7L*(n - 1L)
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
            holidays <- as.Date(holidays)
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

ssm <- function(time, tz = "") {
    if (tz == "UTC" || tz == "GMT")
        as.numeric(time) %% 86400
    else {
        pt <- as.POSIXlt(time, tz)        
        3600 * pt$hour + 60 * pt$min + pt$sec
    }
}
