## -*- truncate-lines: t; -*-
## Time-stamp: <2014-04-03 18:21:29 CEST (es)>

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

    template <-
"%from% -- %to%         (%nobs% obs, %nna% NAs)
Low      %low%  (%low_timestamp%)
High    %high%  (%high_timestamp%)

max DD    %dd%
from    %dd_from%  (%dd_from_timestamp%)
to      %dd_to%    (%dd_to_timestamp%)

vol       %vol%
up        %vol_up%
down      %vol_down%\n"

    ## TODO: assuming daily timestamps -- too restrictive
    timestamp <- aggregate(x$timestamp, by = list(as.Date(x$timestamp)), tail, 1L)[[2L]]
    NAV <- aggregate(x$NAV, by = list(as.Date(x$timestamp)), tail, 1L)[[2L]]

    stats <- list()
    stats$from <- as.character(min(timestamp))
    stats$to   <- as.character(max(timestamp))
    stats$nobs <- length(timestamp)
    stats$nna  <- sum(is.na(NAV))
    stats$low  <- min(NAV)
    stats$high <- max(NAV)

    tmp <- drawdown(x$NAV)
    stats$dd <- format(round(100*tmp$maximum,1), nsmall = 1)
    stats$
    if (length(ii <- which(NAV == stats$low)) > 1L) {
        stats$low_timestamp <- paste0(as.character(timestamp[ii][1L]),
                                      " (several dates)")
        footnote <- TRUE        
    } else
        stats$low_timestamp <- as.character(timestamp[ii])

    if (length(ii <- which(NAV == stats$high)) > 1L) {
        stats$high_timestamp <- paste0(as.character(timestamp[ii][1L]),
                                       " (several dates)")
        footnote <- TRUE        
    } else
        stats$high_timestamp <- as.character(timestamp[ii])

    stats$vol      <- format(round(sd(returns(NAV))*1600,1), nsmall = 1)
    stats$vol_up   <- format(round(pm(returns(NAV), normalise = TRUE, lower = FALSE)*1600,1), nsmall = 1)
    stats$vol_down <- format(round(pm(returns(NAV), normalise = TRUE)*1600,1), nsmall = 1)
    
    for (s in names(stats)) {
        template <- gsub(paste0("%", s, "%"), stats[[s]], template)
    }
    cat(template)
    ## cat("\nMonthly returns in %\n")
    cat("\n")
    print(returns(NAV, timestamp, period = "monthly"), year.rows = FALSE)
    invisible(x)
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

print.NAVseries <- function(x, y, ...) {
    if (missing(y)) {
        par(mar = c(3,3,1,1), las = 1)
        plot(x$timestamp, x$NAV, type = "l")
        

    }
}
