## -*- truncate-lines: t; -*-
## Time-stamp: <2014-02-10 09:00:58 CET (es)>

NAVseries <- function(NAV, timestamp, description = NULL) {
    if (missing(timestamp)) 
        timestamp <- seq_along(NAV)
    ans <- list(NAV = NAV, timestamp = timestamp,
                description = as.character(description))
    class(ans) <- "NAVseries"
    ans
}

print.NAVseries <- function(x, ...) {
    if (!is.null(x$description))
        cat(x$description, "\n\n")
    cat("Min. timestamp: ", min(timestamp),
        " max. timestamp: ", max(timestamp), "\n")

}

plot.NAVseries <- function(x, y, ...) {
    if (!missing(y))
        stop("scatterplot of *returns* -- not implemented")    
    plot(x$timestamp, x$NAV, ...)

    return(invisible(x))
}
