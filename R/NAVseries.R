## -*- truncate-lines: t; -*-
## Time-stamp: <2014-02-11 09:55:10 CET (es)>

NAVseries <- function(NAV, timestamp, description = NULL) {
    if (missing(timestamp)) 
        timestamp <- seq_along(NAV)
    ans <- list(NAV = NAV, timestamp = timestamp,
                description = as.character(description))
    if (any(duplicated(ans$NAV)))
        warning("duplicated timestamps")
    class(ans) <- "NAVseries"
    ans
}

print.NAVseries <- function(x, ...) {
    footnote <- FALSE
    if (!is.null(x$description))
        cat(x$description, "\n")
    timestamp <- aggregate(x$timestamp, by = list(as.Date(x$timestamp)), tail, 1L)[[2L]]
    NAV <- aggregate(x$NAV, by = list(as.Date(x$timestamp)), tail, 1L)[[2L]]
    cat(as.character(min(timestamp)), " => ", as.character(max(timestamp)), "\n")
    minNAV <- min(NAV)
    if (length(ii <- which(NAV == minNAV)) > 1L) {
        minTS <- paste0(as.character(timestamp[ii][1L]), " (several dates)")
        footnote <- TRUE
    } else
        minTS <- as.character(timestamp[ii])
    maxNAV <- max(NAV)
    if (length(ii <- which(NAV == maxNAV)) > 1L) {
        maxTS <- paste0(as.character(timestamp[ii][1L]), " (several dates)")
        footnote <- TRUE
    } else
        maxTS <- as.character(timestamp[ii])
    tmpN <- max(nchar(as.character(minNAV)), nchar(as.character(maxNAV)))
    cat("lowest    ",  format(minNAV, width = tmpN), " @ ", minTS, "\n", sep = "")
    cat("highest   ",  format(maxNAV, width = tmpN), " @ ", maxTS, "\n", sep = "")
    cat("vol (ann.) ", format(round(sd(returns(NAV))*1600,1), nsmall = 1))
    cat("\n")


    
    cat("\nmonthly returns in %\n")
    print(returns(NAV, timestamp, period = "monthly"), year.rows = FALSE)
}

plot.NAVseries <- function(x, y, ...) {
    if (!missing(y))
        stop("scatterplot of *returns* -- not implemented")    
    plot(x$timestamp, x$NAV, ...)

    return(invisible(x))
}

## require("tseries"); require("zoo")
## ## X <- get.hist.quote("AQRIX", quote = "AdjClose")
## x <- NAVseries(coredata(X), index(X), description="AQR Risk Parity")
