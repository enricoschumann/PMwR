## -*- truncate-lines: t; -*-

NAVseries <- function(NAV, timestamp,
                      instrument = NULL,
                      title = NULL,
                      description = NULL) {
    if (missing(timestamp))
        timestamp <- seq_along(NAV)
    ans <- NAV
    attr(ans, "timestamp") <- timestamp
    attr(ans, "instrument") <- instrument
    attr(ans, "title") <- as.character(title)
    attr(ans, "description") <- as.character(description)
    if (anyDuplicated(attr(ans, "timestamp")))
        warning("duplicated timestamps")
    class(ans) <- "NAVseries"
    ans
}

print.NAVseries <- function(x, ...) {
    bm <- function(x)
        if (x >= 10000)
            format(x, big.mark = ",", decimal.mark = ".")
        else
            x
    if (length(title <- attr(x, "title")))
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
        na, " NA", if (na != 1L) "s" ,")\n", sep = "")
    cat(format(first, justify = "right", width = nchar(mint), digits = 6),
        "   ",
        format(last,  justify = "right", width = nchar(maxt), digits = 6))

    ## TODO: should there be warnings if leading or
    ## trailing NAs are removed?
    if (is.na(x[1L]) && is.na(x[n]))
        cat("  (leading and trailing NAs removed)")
    else if (is.na(x[1L]))
        cat("  (leading NAs removed)")
    else if (is.na(x[n]))
        cat("  (trailing NAs removed)")
    cat("\n")
    invisible(x)
}

summary.NAVseries <- function(object, monthly = TRUE, na.rm = FALSE, ...) {
    ## TODO: assuming daily timestamps -- too restrictive? hourly?
    ## TODO: timestamp can also be numeric 1 .. n_obs
    isna <- is.na(object)
    nna <- sum(isna)
    if (na.rm) {
        timestamp <- attr(object, "timestamp")[!isna]
        NAV <- object[!isna]
    } else {
        timestamp <- attr(object, "timestamp")
        NAV <- c(object)
    }
        
    if (!is.null(timestamp) &&
        !inherits(try(timestampD <- as.Date(timestamp),
                      silent = TRUE), "try-error") &&
        !any(is.na(timestampD))) {
        NAV <- aggregate(NAV, by = list(as.Date(timestamp)), tail, 1L)[[2L]]
        timestamp <- aggregate(timestamp,
                               by = list(as.Date(timestamp)), tail, 1L)[[2L]]
    } else
        timestampD <- timestamp

    ans <- list()
    ans$NAVseries <- object
    ans$NAV <- c(object)
    ans$timestamp <- timestamp
    ans$timestamp <- attr(object, "timestamp")
    ans$instrument  <- if (!is.null(attr(object, "instrument")))
                          attr(object, "instrument") else NA
    ans$title       <- if (!is.null(attr(object, "title")))
                          attr(object, "title") else NA
    ans$description <- if (!is.null(attr(object, "description")))
                          attr(object, "description") else NA
    ans$start <- min(timestamp)
    ans$end   <- max(timestamp)
    ans$nobs <- length(attr(object, "timestamp"))
    ans$nna  <- nna
    ans$low  <- min(NAV)
    ans$high <- max(NAV)
    ans$low.when  <- timestamp[which.min(NAV)[1L]]
    ans$high.when <- timestamp[which.max(NAV)[1L]]
    ans$return <- if ((t <- as.numeric(ans$end - ans$start)) > 365)
                      ((tail(NAV,1)/head(NAV,1))^(365/t)) - 1
                  else
                      (tail(NAV,1)/head(NAV,1)) - 1
    ans$return.annualised <- if (t >= 365)
                                 TRUE
                             else
                                 FALSE
    tmp          <- drawdown(NAV)
    ans$mdd      <- tmp$maximum
    ans$mdd.high <- tmp$high
    ans$mdd.low   <- tmp$low
    ans$mdd.high.when <- timestamp[tmp$high.position]
    ans$mdd.low.when   <- timestamp[tmp$low.position]
    ans$underwater <- 1 - NAV[length(NAV)]/max(NAV)

    if (inherits(timestampD, "Date")) {
        if (monthly) {
            tmp <- returns(NAV, t = timestamp, period = "month")
            sq12 <- sqrt(12)
            ans$volatility      <- sd(tmp) * sq12
            ans$volatility.up   <- pm(tmp, normalise = TRUE,
                               lower = FALSE) * sq12
            ans$volatility.down <- pm(tmp, normalise = TRUE) * sq12
        } else {
            ## TODO: assumes daily data -- too restrictive: could be intraday
            ans$volatility      <- sd(returns(NAV))*16
            ans$volatility.up   <- pm(returns(NAV), normalise = TRUE,
                                      lower = FALSE)*16
            ans$volatility.down <- pm(returns(NAV), normalise = TRUE)*16
        }
    } else {
            ans$volatility      <- sd(returns(NAV))
            ans$volatility.up   <- pm(returns(NAV), normalise = TRUE, lower = FALSE)
            ans$volatility.down <- pm(returns(NAV), normalise = TRUE)
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
          "Volatility (%) <>%volatility%|  (annualised)",
          "_ upside <>%volatility.up%|",
          "_ downside <>%volatility.down%|",
          "---------------------------------------------------------\n")
    nx <- names(x)
    nx <- nx[nx != "NAVseries"]
    nx <- nx[nx != "NAV"]
    nx <- nx[nx != "timestamp"]
    nx <- nx[nx != "title"]
    nx <- nx[nx != "description"]
    for (n in nx)
        template <- gsub(paste0("%", n, "%"), x[[n]], template, fixed = TRUE)
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
    NAVseries(NAV = scale1(NAV, level = 100),
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

