## -*- truncate-lines: t; -*-

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
    ## TODO: should there be warnings if leading or trailing NAs are
    ## removed?

    bm <- function(x)
        if (x >= 10000)
            format(x, big.mark = ",", decimal.mark = ".") else x
    if (length(x$title))
        cat(x$title, "\n")
    else if (!is.null(x$instrument))
        cat(x$instrument, "\n")

    timestamp <- x$timestamp
    NAV <- x$NAV
    n <- length(timestamp)
    isna <- is.na(NAV)
    timestamp <- timestamp[!isna]
    NAV <- NAV[!isna]

    if (all(class(timestamp) == "Date")) {
        mint <- format(min(timestamp), "%d %b %Y")
        maxt <- format(max(timestamp), "%d %b %Y")
    } else {
        mint <- as.character(min(timestamp))
        maxt <- as.character(max(timestamp))
    }
    na <- sum(isna)
    first <- NAV[1L]
    last <- NAV[length(NAV)]

    cat(mint, "==>", maxt)
    cat("   (", bm(n), " data points, ", 
        na, " NAs)\n", sep = "")
    cat(format(first, justify = "right", width = nchar(mint), digits = 6), "   ",
        format(last,  justify = "right", width = nchar(maxt), digits = 6), "")
    if (is.na(x$NAV[1L]) && is.na(x$NAV[n]))
        cat("  (leading and trailing NAs removed)")
    else if (is.na(x$NAV[1L]))
        cat("  (leading NAs removed)")
    else if (is.na(x$NAV[n]))
        cat("  (trailing NAs removed)")
    cat("\n")
    invisible(x)
    
}

summary.NAVseries <- function(object, monthly = TRUE, ...) {

    ## TODO: assuming daily timestamps -- too restrictive? hourly?
    ## TODO: timestamp can also be numeric 1 .. n_obs
    isna <- is.na(object$NAV)
    nna <- sum(isna)
    timestamp <- object$timestamp[!isna]
    NAV <- object$NAV[!isna]
    if (!is.null(timestamp) &&
        !inherits(try(timestampD <- as.Date(timestamp), silent = TRUE), "try-error") && 
        !any(is.na(timestampD))) {
        NAV <- aggregate(NAV, by = list(as.Date(timestamp)), tail, 1L)[[2L]]
        timestamp <- aggregate(timestamp,
                               by = list(as.Date(timestamp)), tail, 1L)[[2L]]
    }
    
    ans <- list()
    ans$NAVseries <- object
    ans$NAV <- NAV
    ans$timestamp <- timestamp
    ans$from <- min(timestamp)
    ans$to   <- max(timestamp)
    ans$nobs <- length(object$timestamp)
    ans$nna  <- nna
    ans$low  <- min(NAV)
    ans$high <- max(NAV)
    ans$low.when  <- timestamp[which.min(NAV)[1L]]
    ans$high.when <- timestamp[which.max(NAV)[1L]]
    ans$return <- if ((t <- as.numeric(ans$to - ans$from)) > 365)
                      ((tail(NAV,1)/head(NAV,1))^(365/t)) - 1
                  else 
                      (tail(NAV,1)/head(NAV,1)) - 1
                  
    ans$return.annualised <- if (t > 365) TRUE else FALSE
    tmp          <- drawdown(NAV)
    ans$mdd      <- tmp$maximum
    ans$mdd.high <- tmp$high
    ans$mdd.low   <- tmp$low
    ans$mdd.high.when <- timestamp[tmp$high.position]
    ans$mdd.low.when   <- timestamp[tmp$low.position]
    ans$underwater <- 1 - NAV[length(NAV)]/max(NAV)

    if (monthly) {
        tmp <- last(NAV, format(timestampD, "%Y %m"))
        ans$vol      <- sd(returns(tmp))*sqrt(12)
        ans$vol.up   <- pm(returns(tmp), normalise = TRUE, lower = FALSE)*sqrt(12)
        ans$vol.down <- pm(returns(tmp), normalise = TRUE)*sqrt(12)

    } else {
        ans$vol      <- sd(returns(NAV))*16
        ans$vol.up   <- pm(returns(NAV), normalise = TRUE, lower = FALSE)*16
        ans$vol.down <- pm(returns(NAV), normalise = TRUE)*16
    }
    class(ans) <- "summary.NAVseries"
    ans

}

print.summary.NAVseries <- function(x, ..., sparkplot = TRUE, monthly.returns = TRUE) {
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
          "High <>%high%|  (%high.when%)",
          "Low <>%low%|  (%low.when%)",
          "---------------------------------------------------------",
          "Return (%) <>%return%|  (annualised)",
          "---------------------------------------------------------",
          "Max. drawdown (%)   <> %mdd%|",
          "_ peak <>%mdd.high%|  (%mdd.high.when%)",
          "_ trough <>%mdd.low%|  (%mdd.low.when%)",
          "_ underwater now (%) <>%underwater%|",
          "---------------------------------------------------------",
          "Volatility (%) <>%vol%|  (annualised)",
          "_ upside <>%vol.up%|",
          "_ downside <>%vol.down%|",
          "---------------------------------------------------------\n")
    nx <- names(x)
    nx <- nx[nx != "NAVseries"]
    nx <- nx[nx != "NAV"]
    nx <- nx[nx != "timestamp"]
    for (n in nx)
        template <- gsub(paste0("%", n, "%"), x[[n]], template)
    template <- valign(template)
    cat(template, sep = "\n")
    if (monthly.returns && inherits(x$timestamp, "Date")) {        
        cat("Monthly returns  ")
        if (.Platform$OS.type == "unix") 
            sparkplot(returns(x=x$NAV, t=x$timestamp, period = "month"))
        cat("\n")
        
        print(returns(x$NAV, x$timestamp, period = "month"),
              year.rows = TRUE)
    }
    invisible(x)
}

toLatex.summary.NAVseries <- function(x, ...) {
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

    ## time period   from -- to
    ## return p.a.
    ## Volatility (%)           4.2  (annualised)
    ## _ upside                 3.2
    ## _ downside               2.7
    ## max drawdown %
    ## _ peak                139.17  (16 May 2013)
    ## _ trough              132.69  (26 Dec 2014)
    ## _ underwater as of, %)     0.1
    ## ---------------------------------------------------------

    template <-
        c("---------------------------------------------------------",
          "High <>%high%|  (%high.when%)",
          "Low <>%low%|  (%low.when%)",
          "---------------------------------------------------------",
          "Return (%) <>%return%|  (annualised)",
          "---------------------------------------------------------",
          "Max. drawdown (%)   <> %mdd%|",
          "_ peak <>%mdd.high%|  (%mdd.high.when%)",
          "_ trough <>%mdd.low%|  (%mdd.low.when%)",
          "_ underwater now (%) <>%underwater%|",
          "---------------------------------------------------------",
          "Volatility (%) <>%vol%|  (annualised)",
          "_ upside <>%vol.up%|",
          "_ downside <>%vol.down%|",
          "---------------------------------------------------------\n")
    nx <- names(x)
    nx <- nx[nx != "NAVseries"]
    nx <- nx[nx != "NAV"]
    nx <- nx[nx != "timestamp"]
    for (n in nx)
        template <- gsub(paste0("%", n, "%"), x[[n]], template)
    template <- valign(template)
    cat(template, sep = "\n")
    if (monthly.returns && inherits(x$timestamp, "Date")) {        
        cat("Monthly returns  ")
        if (.Platform$OS.type == "unix") 
            sparkplot(returns(x=x$NAV, t=x$timestamp, period = "month"))
        cat("\n")
        
        print(returns(x$NAV, x$timestamp, period = "month"),
              year.rows = TRUE)
    }
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

as.NAVseries.btest <- function(x, ...){
    NAV <- x$wealth
    if (any(na <- is.na(NAV))) {
        leading.na <- 1:max(which(na))
        NAV <- NAV[-leading.na]
    }
    timestamp <- if (!is.null(x$timestamp))
                     x$timestamp[-leading.na] else seq_along(NAV)
    NAVseries(NAV = scale1(NAV, level = 100),
              timestamp = timestamp)
}

as.zoo.NAVseries <- function(x, ...){
    zoo(x$NAV, x$timestamp)
}

.summary.NAVseries.template <- 
"\\begin{tabular}{lrl}
\\multicolumn{3}{l}{NAV series starts %from%, ends %to%}                         \\\\[-0.25ex]
\\multicolumn{3}{l}{\\footnotesize(%nobs% oberservations, no missing values)}    \\\\[1ex]
High                   & %high%    & \\footnotesize(%high.when%)                 \\\\
Low                    & %low%     & \\footnotesize(%low.when%)                  \\\\[1ex]
Return p.a. in \\%     & %return%  &                                             \\\\[1ex]
Drawdown                                                                         \\\\
\\quad maximum         & %mdd%\\%                                                \\\\
\\quad peak            & %mdd.high%   & \\footnotesize(%mdd.high.when%)          \\\\
\\quad trough          & %mdd.low%    & \\footnotesize(%mdd.low.when%)           \\\\
\\quad underwater  now & %underwater% &                                          \\\\[1ex]
Volatility p.a. in \\% & %vol%        &                                          \\\\
\\quad upside          & %vol.up%                                                \\\\ 
\\quad downside        & %vol.down%                                              \\\\
\\end{tabular}"

toLatex.summary.NAVseries <-
    function(x, template = .summary.NAVseries.template, ...) {

.summary.NAVseries.template        
}
    
