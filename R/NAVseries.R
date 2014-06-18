## -*- truncate-lines: t; -*-
## Time-stamp: <2014-06-18 20:06:58 CEST (es)>

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
    first <- x$NAV[1]
    last <- x$NAV[n]

    cat(mint, "==>", maxt)
    cat("   (",
        format(n, big.mark = "."),
        " data points, ", 
        na,
        " NAs)\n",
        sep = "")
    cat(format(first, justify = "right", width = nchar(mint)), "   ",
        format(last, justify = "right", width = nchar(maxt)), "\n")
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
    ans$nobs <- format(length(timestamp), big.mark = ".")
    ans$nna  <- sum(is.na(NAV))
    ans$low  <- min(NAV)
    ans$high <- max(NAV)
    
    tmp <- drawdown(object$NAV)
    ans$dd      <- tmp$maximum
    ans$dd_from <- tmp$high
    ans$dd_to   <- tmp$low
    ans$dd_from_timestamp <- datef(timestamp[tmp$high.position])
    ans$dd_to_timestamp <- datef(timestamp[tmp$low.position])
    ans$uw <- percf(1 - tail(NAV,1)/max(NAV))


    class(ans) <- "summary.NAVseries"
    ans

}

print.summary.NAVseries <- function(x, ...) {
    class(x) <- "NAVseries"
    print(x)
    invisible(x)

    datef <- function(x)
        format(x, "%d %b %Y")
    percf <- function(x)
        format(round(100*x, 1), nsmall = 1, justify = "right", width = 7)
    numf <- function(x)
        format(x, justify = "right", width = 7)

    if (length(x$title))
        cat(x$title, "\n")
    else if (!is.null(x$instrument))
        cat(x$instrument, "\n")

    ans <- list()
    ans$NAVseries <- object
    stats <- list()
    stats$from <- datef(min(timestamp))
    stats$to   <- datef(max(timestamp))
    stats$nobs <- format(length(timestamp), big.mark = ".")
    stats$nna  <- sum(is.na(NAV))
    stats$low  <- min(NAV)
    stats$high <- max(NAV)
    
    tmp <- drawdown(object$NAV)
    stats$dd <- percf(tmp$maximum)
    stats$dd_from <- tmp$high
    stats$dd_to <- tmp$low
    stats$dd_from_timestamp <- datef(timestamp[tmp$high.position])
    stats$dd_to_timestamp <- datef(timestamp[tmp$low.position])
    stats$uw <- percf(1 - tail(NAV,1)/max(NAV))


    
    template <-
        c("%from% --> %to%         (%nobs% obs, %nna% NAs)",
          "",
          "Low    |  %low% | /%low_timestamp%",
          "High   | %high% | /%high_timestamp%",
          "",
          "drawdown    %dd%                 vol   %vol%",
          "from        %dd_from%  /%dd_from_timestamp%     up   %vol_up%",
          "to          %dd_to%  /%dd_to_timestamp%     down %vol_down%",
          "underwater  %uw%\n")

}

print.NAVseries <- function(x, ...) {
        
    
    if (length(ii <- which(NAV == stats$low)) > 1L) {
        stats$low_timestamp <- paste0(datef(timestamp[ii][1L]), "*")
        footnote <- TRUE        
    } else
        stats$low_timestamp <- datef(timestamp[ii])

    if (length(ii <- which(NAV == stats$high)) > 1L) {
        stats$high_timestamp <- paste0(datef(timestamp[ii][1L]), "*")
        footnote <- TRUE        
    } else
        stats$high_timestamp <- datef(timestamp[ii])

    stats$vol      <- percf(sd(returns(NAV))*16)
    stats$vol_up   <- percf(pm(returns(NAV), normalise = TRUE, lower = FALSE)*16)
    stats$vol_down <- percf(pm(returns(NAV), normalise = TRUE)*16)
    
    for (s in names(stats)) {
        template <- gsub(paste0("%", s, "%"), stats[[s]], template)
    }
    
    cat(template, fill = TRUE)
    
    ## cat("\nMonthly returns in %\n")

    ## in_lines <- strsplit(template, "\n", TRUE)[[1L]]
    ## in_lines[1] <- expstr(in_lines[1], after = "  +", width = 55)

    cat("\n")
    ##print(returns(NAV, timestamp, period = "monthly"), year.rows = FALSE)
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

## print.NAVseries <- function(x, y, ...) {
##     if (missing(y)) {
##         par(mar = c(3,3,1,1), las = 1)
##         plot(x$timestamp, x$NAV, type = "l")
        

##     }
## }
