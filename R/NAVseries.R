## -*- truncate-lines: t; -*-
## Time-stamp: <2014-11-14 16:14:56 CET (es)>

NAVseries <- function(NAV, timestamp,
                      instrument = NULL,
                      title = NULL,
                      description = NULL) {
    if (missing(timestamp)) 
        timestamp <- seq_along(NAV)
    ans <- list(NAV = NAV, timestamp = timestamp,
                instrument = instrument,
                title = as.character(title),
                description = as.character(description))
    if (any(duplicated(ans$timestamp)))
        warning("duplicated timestamps")
    class(ans) <- "NAVseries"
    ans
}


print.NAVseries <- function(x, ...) {
    if (length(x$title))
        cat(x$title, "\n")
    else if (!is.null(x$instrument))
        cat(x$instrument, "\n")

    if (all(class(x$timestamp) == "Date")) {
        mint <- format(min(x$timestamp), "%d %b %Y")
        maxt <- format(max(x$timestamp), "%d %b %Y")
    } else {
        mint <- as.character(min(x$timestamp))
        maxt <- as.character(max(x$timestamp))
    }
    n <- length(x$timestamp)
    na <- sum(is.na(x$NAV))
    first <- x$NAV[1L]
    last <- x$NAV[n]

    cat(mint, "==>", maxt)
    cat("   (", format(n, big.mark = "."), " data points, ", 
        na, " NAs)\n", sep = "")
    cat(format(first, justify = "right", width = nchar(mint), digits = 2), "   ",
        format(last,  justify = "right", width = nchar(maxt), digits = 2), "\n")
    invisible(x)
    
}

summary.NAVseries <- function(object, ...) {

    ## FIXME: assuming daily timestamps -- too restrictive? hourly?
    timestamp <- aggregate(object$timestamp,
                           by = list(as.Date(object$timestamp)), tail, 1L)[[2L]]
    NAV <- aggregate(object$NAV, by = list(as.Date(object$timestamp)), tail, 1L)[[2L]]

    ans <- list()
    ans$NAVseries <- object

    ans$from <- min(timestamp)
    ans$to   <- max(timestamp)
    ans$nobs <- length(timestamp)
    ans$nna  <- sum(is.na(NAV))
    ans$low  <- min(NAV)
    ans$high <- max(NAV)
    ans$low.when  <- timestamp[which.min(NAV)[1L]]
    ans$high.when <- timestamp[which.max(NAV)[1L]]
    ans$return <- if ((t <- as.numeric(ans$to - ans$from)) > 365)
                      ((tail(NAV,1)/head(NAV,1))^(365/t)) - 1
                  else 
                      (tail(NAV,1)/head(NAV,1)) - 1
                  
    ans$return.annualised <- if (t > 365) TRUE else FALSE
    tmp          <- drawdown(object$NAV)
    ans$mdd      <- tmp$maximum
    ans$mdd.high <- tmp$high
    ans$mdd.low   <- tmp$low
    ans$mdd.high.when <- timestamp[tmp$high.position]
    ans$mdd.low.when   <- timestamp[tmp$low.position]
    ans$underwater <- 1 - NAV[ans$nobs]/max(NAV)

    ans$vol      <- sd(returns(NAV))*16
    ans$vol.up   <- pm(returns(NAV), normalise = TRUE, lower = FALSE)*16
    ans$vol.down <- pm(returns(NAV), normalise = TRUE)*16

    class(ans) <- "summary.NAVseries"
    ans

}

print.summary.NAVseries <- function(x, ...) {
    datef <- function(x) {
        if (inherits(x[1L], "Date"))
            x <- format(x, "%d %b %Y")
        x
    }
    percf <- function(x)
        format(round(100*x, 1), nsmall = 1, justify = "right", width = 7)
    numf <- function(x)
        format(x, justify = "right", width = 7, nsmall = 2, digits = 2,
               scientific = FALSE)
    if (length(x$NAVseries$title))
        cat(x$NAVseries$title, "\n")
    else if (!is.null(x$NAVseries$instrument))
        cat(x$NAVseries$instrument, "\n")

    ## format: dates
    fields <- c("from", "to", "low.when", "high.when",
                "mdd.high.when", "mdd.low.when")
    for (f in fields)
        x[[f]] <- datef(x[[f]])

    ## format: %
    fields <- c("mdd", "underwater",
                "vol", "vol.up", "vol.down", "return")
    for (f in fields)
        x[[f]] <- percf(x[[f]])

    ## format: prices
    fields <- c("high", "low", "mdd.high", "mdd.low")
    for (f in fields)
        x[[f]] <- numf(x[[f]])

    
    cat("---------------------------------------------------------\n")
    print(x$NAVseries)    
    template <-
        c("---------------------------------------------------------",
          "High         %high%  (%high.when%)",
          "Low           %low%  (%low.when%)",
          "---------------------------------------------------------",
          "return (%)       %return%",
          "---------------------------------------------------------",
          "max. drawdown (in %)  %mdd%",
          "peak          %mdd.high%  (%mdd.high.when%)",
          "trough        %mdd.low%  (%mdd.low.when%)",
          "underwater (in %) %underwater%",
          "---------------------------------------------------------",
          "volatility (in %, ann.)  %vol%",
          "upside volatility        %vol.up%",
          "downside volatility      %vol.down%",
          "---------------------------------------------------------\n")
    nx <- names(x)
    nx <- nx[nx != "NAVseries"]
    for (n in nx)
        template <- gsub(paste0("%", n, "%"), x[[n]], template)
    cat(template, sep = "\n")
    cat("Monthly returns  ")
    sparkplot(returns(x$NAVseries$NAV))
    cat("\n")
    print(returns(x$NAVseries$NAV, x$NAVseries$timestamp, period = "month"),
          year.rows = FALSE)
    invisible(x)

}

## print.NAVseries <- function(x, ...) {
        
    
##     if (length(ii <- which(NAV == stats$low)) > 1L) {
##         stats$low_timestamp <- paste0(datef(timestamp[ii][1L]), "*")
##         footnote <- TRUE        
##     } else
##         stats$low_timestamp <- datef(timestamp[ii])

##     if (length(ii <- which(NAV == stats$high)) > 1L) {
##         stats$high_timestamp <- paste0(datef(timestamp[ii][1L]), "*")
##         footnote <- TRUE        
##     } else
##         stats$high_timestamp <- datef(timestamp[ii])

##     stats$vol      <- percf(sd(returns(NAV))*16)
##     stats$vol_up   <- percf(pm(returns(NAV), normalise = TRUE, lower = FALSE)*16)
##     stats$vol_down <- percf(pm(returns(NAV), normalise = TRUE)*16)
    
##     for (s in names(stats)) {
##         template <- gsub(paste0("%", s, "%"), stats[[s]], template)
##     }
    
##     cat(template, fill = TRUE)
    
##     cat("\n")
##     invisible(x)
## }


plot.NAVseries <- function(x, y, ...) {
    if (!missing(y))
        stop("scatterplot of *returns* -- not implemented")    
    plot(x=x$timestamp, y=x$NAV, ...)

    return(invisible(x))
}

as.NAVseries <- function(x, ...) {
    UseMethod("as.NAVseries")
}

as.NAVseries.zoo <- function(x, ...){
    dx <- dim(x)
    if (!is.null(dx) && !any(dx == 1))
        stop("can only coerce single series")
    NAVseries(NAV = coredata(x), timestamp = index(x))
}

as.zoo.NAVseries <- function(x, ...){
    zoo(x$NAV, x$timestamp)
}
