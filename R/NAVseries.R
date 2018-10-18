## -*- truncate-lines: t; -*-
## Copyright (C) 2008-18  Enrico Schumann

NAVseries <- function(NAV, timestamp,
                      instrument = NULL,
                      title = NULL,
                      description = NULL) {
    if (missing(timestamp))
        timestamp <- seq_along(NAV)
    else if (anyDuplicated(timestamp))
        warning("duplicated timestamps")

    if (length(NAV) < length(timestamp))
        NAV <- rep(NAV, length(timestamp)/length(NAV))
    else if (length(NAV) > length(timestamp))
        warning("length(NAV) > length(timestamp)")
        
    ans <- NAV
    attr(ans, "timestamp") <- timestamp
    attr(ans, "instrument") <- instrument
    attr(ans, "title") <- title
    attr(ans, "description") <- as.character(description)
    class(ans) <- "NAVseries"
    ans
}


.bigmark <- function(x) {
    if (x >= 10000)
        format(x, big.mark = ",", decimal.mark = ".")
    else
        x
}

print.NAVseries <- function(x, ...) {
    if (!is.null(title <- attr(x, "title")))
        cat(title, "\n")
    else if (!is.null(instrument <- attr(x, "instrument")))
        cat(instrument, "\n")

    timestamp <- attr(x, "timestamp")
    NAV <- x
    n <- length(timestamp)
    isna <- is.na(NAV)
    timestamp <- timestamp[!isna]
    NAV <- NAV[!isna]

    if (all(class(timestamp) == "Date")) {
        mint <- format(min(timestamp, na.rm = TRUE), "%d %b %Y")
        maxt <- format(max(timestamp, na.rm = TRUE), "%d %b %Y")
    } else {
        mint <- as.character(min(timestamp, na.rm = TRUE))
        maxt <- as.character(max(timestamp, na.rm = TRUE))
    }
    na <- sum(isna)
    first <- NAV[1L]
    last <- NAV[length(NAV)]

    ## TODO: use template
    ## .template <-
    ## c("       2 ==>   4  | 4 data points, 2 NAs",
    ##   "NAV  100 ==> 100  | leading NAs removed")
    ## cat(paste(.template, collapse = "\n"))
    ##
    ##        2 ==>   4  (4 data points, 2 NAs)
    ## NAV  100 ==> 100  (leading NAs removed)

    cat(mint, "==>", maxt)
    cat("   (", .bigmark(n), " data points, ",
        na, " NA", if (na != 1L) "s" ,")\n", sep = "")
    cat(format(first, justify = "right", width = nchar(mint), digits = 6),
        "   ",
        format(last,  justify = "right", width = nchar(maxt), digits = 6))

    if (is.na(x[1L]) && is.na(x[n]))
        cat("  (leading and trailing NAs removed)")
    else if (is.na(x[1L]))
        cat("  (leading NAs removed)")
    else if (is.na(x[n]))
        cat("  (trailing NAs removed)")
    cat("\n")
    invisible(x)
}

summary.NAVseries <- function(object, ...,
                              monthly.vol = TRUE,
                              na.rm = FALSE,
                              assume.daily = FALSE) {
    ## TODO: assuming daily timestamps -- too restrictive? hourly?
    ## TODO: timestamp can also be numeric 1 .. n_obs
    isna <- is.na(object)
    nna <- sum(isna)
    nobs <- length(attr(object, "timestamp"))    
    if (na.rm) {
        timestamp <- attr(object, "timestamp")[!isna]
        NAV <- object[!isna]
    } else {
        timestamp <- attr(object, "timestamp")
        NAV <- c(object)
    }

    ## TODO: argument 'aggregate': daily, ...?
    ## if (!is.null(timestamp) &&
    ##     !inherits(try(timestampD <- as.Date(timestamp),
    ##                   silent = TRUE), "try-error") &&
    ##     !any(is.na(timestampD))) {
    ##     NAV <- aggregate(NAV, by = list(as.Date(timestamp)), tail, 1L)[[2L]]
    ##     timestamp <- aggregate(timestamp,
    ##                            by = list(as.Date(timestamp)), tail, 1L)[[2L]]
    ## } else
    ##     timestampD <- timestamp

    ans <- list()
    ans$NAVseries <- object
    ans$NAV <- NAV
    ans$timestamp <- timestamp
    ans$instrument  <- if (!is.null(attr(object, "instrument")))
                          attr(object, "instrument") else NA
    ans$title       <- if (!is.null(attr(object, "title")))
                          attr(object, "title") else NA
    ans$description <- if (!is.null(attr(object, "description")))
                          attr(object, "description") else NA
    ans$start <- min(timestamp)
    ans$end   <- max(timestamp)
    ans$nobs <- nobs
    ans$nna  <- nna
    ans$low  <- min(NAV)
    ans$high <- max(NAV)
    ans$low.when  <- timestamp[which.min(NAV)[1L]]
    ans$high.when <- timestamp[which.max(NAV)[1L]]


    
    ## Returns, annualise
    if (.may_be_Date(c(ans$start, ans$end))) {
        t <- as.numeric(as.Date(ans$end) -
                        as.Date(ans$start))
    } else if (assume.daily)
        t <- ans$end - ans$start
    else
        t <- NA

    if (!is.na(t) && t > 365) {
        ans$return <- (tail(NAV,1)/head(NAV, 1))^(365/t) - 1
        ans$return.annualised <- TRUE
    } else {
        ans$return <- tail(NAV,1)/head(NAV,1) - 1
        ans$return.annualised <- FALSE            
    }
        
    tmp <- drawdowns(NAV)    
    dd.row <- if(nrow(tmp))
                  which.max(tmp[["max"]])
              else
                  0
    if (dd.row > 0) {
        ans$mdd      <- tmp[["max"]][dd.row]
        ans$mdd.high <- NAV[tmp[["peak"]][dd.row]]
        ans$mdd.low <- NAV[tmp[["trough"]][dd.row]]
        ans$mdd.high.when <- timestamp[tmp[["peak"]][dd.row]]
        ans$mdd.low.when <- timestamp[tmp[["trough"]][dd.row]]
        ans$mdd.recover.when <- timestamp[tmp[["recover"]][dd.row]]
    } else {
        ans$mdd      <- 0
        ans$mdd.high <- max(NAV)
        ans$mdd.low   <- NA
        ans$mdd.high.when <- timestamp[which.max(NAV)]
        ans$mdd.low.when   <- NA
        ans$mdd.recover.when <- NA
    }
    ans$underwater <- 1 - NAV[length(NAV)]/max(NAV)
    
    if (.may_be_Date(timestamp) && monthly.vol) {
        tmp <- returns(NAV, t = timestamp, period = "month")
        sq12 <- sqrt(12)
        ans$volatility <- sd(tmp) * sq12
        ans$volatility.up <- pm(tmp,
                                normalise = TRUE,
                                lower = FALSE) * sq12
        ans$volatility.down <- pm(tmp, normalise = TRUE) * sq12
    } else if (assume.daily) {
        ans$volatility <- sd(returns(NAV))*16
        ans$volatility.up <- pm(returns(NAV),
                                normalise = TRUE,
                                lower = FALSE)*16
        ans$volatility.down <- pm(returns(NAV),
                                  normalise = TRUE)*16
    } else {
            ans$volatility      <- sd(returns(NAV))
            ans$volatility.up   <- pm(returns(NAV),
                                      normalise = TRUE,
                                      lower = FALSE)
            ans$volatility.down <- pm(returns(NAV),
                                      normalise = TRUE)
    }

    class(ans) <- "summary.NAVseries"
    ans
}

print.summary.NAVseries <- function(x, ...,
                                    sparkplot = TRUE,
                                    monthly.returns = TRUE) {
    datef <- function(x) {
        if (inherits(x[1L], "Date"))
            x <- format(x, "%d %b %Y")
        else if (inherits(x[1L], "POSIXt"))
            x <- format(x, "%d %b %Y %H:%M")
        x
    }
    percf <- function(x)
        format(round(100*x, 1), nsmall = 1, justify = "right", width = 7)
    numf <- function(x)
        format(x, justify = "right", width = 7, nsmall = 2, digits = 2,
               scientific = FALSE)

    ## format: dates
    fields <- c("from", "to", "low.when", "high.when",
                "mdd.high.when", "mdd.low.when",
                "mdd.recover.when")
    for (f in fields)
        x[[f]] <- datef(x[[f]])

    ## format: %
    fields <- c("mdd", "underwater",
                "volatility", "volatility.up", "volatility.down", "return")
    for (f in fields)
        x[[f]] <- percf(x[[f]])

    ## format: prices
    fields <- c("high", "low", "mdd.high", "mdd.low")
    for (f in fields)
        x[[f]] <- numf(x[[f]])

    cat("---------------------------------------------------------\n")
    print(x$NAVseries, ...)
    template <-
        c("---------------------------------------------------------",
          "High<>%high%|  (%high.when%)",
          "Low<>%low%|  (%low.when%)",
          "---------------------------------------------------------",
          "Return (%)<>%return%|  (annualised)",
          "---------------------------------------------------------",
          "Max. drawdown (%)  <> %mdd%|",
          "_ peak<>%mdd.high%|  (%mdd.high.when%)",
          "_ trough<>%mdd.low%|  (%mdd.low.when%)",
          "_ recovery<>|  (%mdd.recover.when%)",
          "_ underwater now (%)<>%underwater%|",
          "---------------------------------------------------------",
          "Volatility (%)<>%volatility%|  (annualised)",
          "_ upside<>%volatility.up%|",
          "_ downside<>%volatility.down%|",
          "---------------------------------------------------------\n")
    nx <- names(x)
    for (r in c("NAVseries", "NAV",
                "timestamp", "title",
                "description"))
        nx <- nx[nx != r]
    for (n in nx)
        template <- gsub(paste0("%", n, "%"),
                         if (is.na(x[[n]])) "NA" else x[[n]],
                             template, fixed = TRUE)
    template <- valign(template)
    if (!x$return.annualised)
        template <- sub("(annualised)", "", template, fixed = TRUE)
    cat(template, sep = "\n")
    if (monthly.returns && inherits(x$timestamp, "Date")) {
        cat("Monthly returns  ")
        mr <- returns(x = x$NAV,
                      t = x$timestamp,
                      period = "month")
        if (.Platform$OS.type == "unix")
            sparkplot(mr)
        cat("\n")
        print(mr, ...)
    }
    invisible(x)
}


toLatex.summary.NAVseries <- function(object, ...,
                                      template = " %title & %return & %volatility & %sparkline \\\\",
                                      file = NULL) {
    dots <- c(list(object), list(...))

    fmt_p <- function(x, ...) {
        if (is.numeric(x))
            format(round(x*100, 1), nsmall = 1)
        else x
    }
    ns <- length(dots)
    ans <- if (length(template) == 1L) 
        rep(template, ns)
    else
        template
    
    fields <- c("instrument", "title", "description", 
                "start", "end", "nobs", "nna",
                "low.when", "high.when", "low", "high", 
                "return.annualised", "return", 
                "mdd.high.when", "mdd.low.when",
                "mdd.high", "mdd.low",
                "mdd", "underwater",
                "volatility.up", "volatility.down", "volatility")

    perc_fields <- c("return", 
                     "mdd", "underwater",
                     "volatility", "volatility.up", "volatility.down")

    for (field in fields) {
        field_values <- unlist(lapply(dots, `[[`, field))
        if (is.null(field_values) ||
            length(field_values) == 0L ||
            all(is.na(field_values)))
            field_values <- rep("NA", ns)
        else if (field %in% perc_fields)
            field_values <- fmt_p(field_values)
        for (i in seq_len(ns))
            ans[i] <- gsub(paste0("%", field), field_values[i], ans[i],
                           fixed = TRUE)
    }

    NAVs <- lapply(dots, `[[`, "NAV")
    NAVs <- lapply(NAVs, scale1)
    if (any(grepl("%sparkline", template, fixed = TRUE))) {
        
        true.min <- min(unlist(NAVs))
        true.max <- max(unlist(NAVs))        
        for (i in seq_len(ns)) {            
            ans[i] <- gsub("%sparkline",
                           paste(sparkline(NAVs[[i]],
                                           true.min = true.min,
                                           true.max = true.max,
                                           height = 2.5,
                                           width = 10),
                                 collapse = "\n"),
                           ans[i], fixed = TRUE)
        }
        
        
    }
    class(ans) <- "Latex"
    if (!is.null(file)) {
        writeLines(ans, con = file)
        invisible(ans)
    } else
        ans
}

plot.NAVseries <- function(x, y, ..., 
                           xlab = "", ylab = "", type = "l") {
    if (!missing(y))
        stop("scatterplot of *returns* -- not implemented")
    plot(x = attr(x, "timestamp"),
         y = x, type = type,
         xlab = xlab, ylab = ylab, ...)

    invisible()
}

as.NAVseries <- function(x, ...) {
    UseMethod("as.NAVseries")
}

as.NAVseries.NAVseries <- function(x, ...) {
    x
}

as.NAVseries.zoo <- function(x,
                             instrument = NULL,
                             title = NULL,
                             description = NULL, ...){
    dx <- dim(x)
    if (!is.null(dx) && !any(dx == 1L))
        stop("can only coerce a _single_ series")
    NAVseries(NAV = coredata(x), timestamp = index(x),
              instrument = instrument,
              title = title,
              description = description)
}

as.NAVseries.btest <- function(x, ...,
                               drop.NA = TRUE){
    NAV <- x$wealth
    timestamp <- if (!is.null(x$timestamp))
                     x$timestamp
                 else
                     seq_along(NAV)
    if (drop.NA && is.na(NAV[1L])) {
        leading.na <- min(which(!is.na(NAV))) - 1
        leading.na <- seq_len(leading.na)
        NAV <- NAV[-leading.na]
        timestamp <- timestamp[-leading.na]
    }
    NAVseries(NAV = NAV,
              timestamp = timestamp)
}

as.zoo.NAVseries <- function(x, ...){
    zoo(x, attr(x, "timestamp"))
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

window.NAVseries <- function(x, start = NULL, end = NULL, ...) {

    timestamp <- .timestamp(x)

    ## TODO frequencies: monthly etc?
    if (is.null(start))
        start <- .timestamp(x)[1L]
    else if (.may_be_Date(timestamp) &&
             grepl("^[12][0-9][0-9][0-9]$", trim(as.character(start))))
        start <- as.Date(paste0(start, "-1-1"))
    
    if (is.null(end))
        end <- .timestamp(x)[length(.timestamp(x))]
    else if (.may_be_Date(timestamp) &&
             grepl("^[12][0-9][0-9][0-9]$", trim(as.character(end))))
        end <- as.Date(paste0(end, "-12-31"))

    if (start > end) 
        stop(sQuote("start"), " cannot be after ", sQuote("end"))

    i <- which(timestamp == start)[1L]
    j <- tail(which(timestamp == end), 1)

    ans <- x[i:j]
    attributes(ans) <- attributes(x)
    .timestamp(ans) <- .timestamp(ans)[i:j]
    ans
    
}
