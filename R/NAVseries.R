## -*- truncate-lines: t; -*-
## Time-stamp: <2014-02-28 17:27:02 CET (es)>

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
        cat(x$title, "\n") else if (!is.null(x$instrument))
        cat(x$instrument, "\n")

    ## TODO: assuming daily timestamps -- too restrictive
    timestamp <- aggregate(x$timestamp, by = list(as.Date(x$timestamp)), tail, 1L)[[2L]]
    NAV <- aggregate(x$NAV, by = list(as.Date(x$timestamp)), tail, 1L)[[2L]]
    cat(as.character(min(timestamp)), " to ", as.character(max(timestamp)), "  |  ",
        length(timestamp), " observations  |  ", sum(is.na(NAV)), " NAs\n", sep = "")
    minNAV <- min(NAV)
    if (length(ii <- which(NAV == minNAV)) > 1L) {
        minTS <- paste0(as.character(timestamp[ii][1L]), " (several dates)")
        footnote <- TRUE
    } else
        minTS <- as.character(timestamp[ii])
    maxNAV <- max(NAV)
    if (length(ii <- which(NAV == maxNAV)) > 1L) {
        maxTS <- paste0(as.character(timestamp[ii][1L]), ", several dates")
        footnote <- TRUE
    } else
        maxTS <- as.character(timestamp[ii])
    tmpN <- max(nchar(as.character(minNAV)), nchar(as.character(maxNAV)))
    cat("low  ",  format(minNAV, width = tmpN), " [", minTS, "]  |  ", sep = "")
    cat("high ",  format(maxNAV, width = tmpN), " [", maxTS, "]\n", sep = "")
    cat("vol (ann.) ", format(round(sd(returns(NAV))*1600,1), nsmall = 1))
    cat("\n")

        cat("\nmonthly returns in %\n")
    print(returns(NAV, timestamp, period = "monthly"), year.rows = FALSE)

}

summary.NAVseries <- function(object, ...) {

    ans <- object
    class(ans) <- "summary.NAVseries"
    ans

}

print.summary.NAVseries <- function(x, ...) {
    class(x) <- "NAVseries"
    print(x)
    invisible(x)
}

plot.NAVseries <- function(x, y, ...) {
    if (!missing(y))
        stop("scatterplot of *returns* -- not implemented")    
    plot(x$timestamp, x$NAV, ...)

    return(invisible(x))
}

## require("tseries"); require("zoo")
## X <- get.hist.quote("AQRIX", quote = "AdjClose")
## x <- NAVseries(coredata(X), index(X), instrument = "AQR Risk Parity")
