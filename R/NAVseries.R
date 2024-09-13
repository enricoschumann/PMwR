## -*- truncate-lines: t; -*-
## Copyright (C) 2008-24  Enrico Schumann

NAVseries <- function(NAV,
                      timestamp,
                      instrument = NULL,
                      title = NULL,
                      description = NULL,
                      drop.NA = NULL) {

    ## 'NAVseries(<btest>)' is natural since the NAV series
    ## ('wealth') is only extracted from the 'btest'
    ## object, i.e. it is not coerced. But this seems a
    ## rare case, so a generic should not be required.
    if (inherits(NAV, "btest"))
        return(as.NAVseries.btest(
            NAV,
            instrument = instrument,
            title = title,
            description = description,
            drop.NA = if (is.null(drop.NA))
                          TRUE else FALSE))


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

.hl.char <-
    ifelse(.Platform$OS.type != "windows", "\u2014", "-")

.bigmark <- function(x) {
    if (x >= 10000)
        format(x, big.mark = ",", decimal.mark = ".")
    else
        x
}

.header <- function(h, width = 55,
                    line = .hl.char,
                    open = " [ ",
                    close = " ] ",
                    line.start = "",
                    line.end = "") {

    nc <- nchar(h)
    left <- width - (nc + nchar(line.start) + nchar(line.end) +
                     nchar(open) + nchar(close))
    lines <- character(length(h))
    for (i in 1:length(h)) {
        lines[i] <- paste0(rep(line, trunc(left[[i]]/2)), collapse = "")
    }
    paste0(line.start,
           ifelse(left %% 2, " ", ""),
           lines, open, h, close, lines,
           line.end)
}


print.NAVseries <- function(x, ..., na.rm = FALSE) {
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
        mint <- format(min(timestamp, na.rm = na.rm), "%d %b %Y")
        maxt <- format(max(timestamp, na.rm = na.rm), "%d %b %Y")
    } else {
        mint <- as.character(min(timestamp, na.rm = na.rm))
        maxt <- as.character(max(timestamp, na.rm = na.rm))
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
                              bm = NULL,
                              monthly.te = TRUE,
                              na.rm = FALSE,
                              assume.daily = FALSE) {

    all_series <- if (missing(object) &&
                      length(list(...)) > 0)
                      list(...)
                  else
                      c(list(object), list(...))
    has.bm <- FALSE
    if (!is.null(bm)) {
        has.bm <- TRUE
        if (!inherits(bm, "NAVseries"))
            bm <- as.NAVseries(bm)

        if (na.rm) {
            bm.timestamp <- attr(bm, "timestamp")[!isna]
            bm.NAV <- bm[!isna]
        } else {
            bm.timestamp <- attr(bm, "timestamp")
            bm.NAV <- c(bm)
        }
        all_series <- c(all_series, list(bm))
    }
    ans <- vector("list", length = length(all_series))

    for (i in seq_along(ans)) {

        ## TODO: assuming daily timestamps -- too restrictive? hourly?
        ## TODO: timestamp can also be numeric 1 .. n_obs
        isna <- is.na(all_series[[i]])
        nna <- sum(isna)
        nobs <- length(attr(all_series[[i]], "timestamp"))
        if (na.rm) {
            timestamp <- attr(all_series[[i]], "timestamp")[!isna]
            NAV <- all_series[[i]][!isna]
        } else {
            timestamp <- attr(all_series[[i]], "timestamp")
            NAV <- c(all_series[[i]])
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

        ans1 <- list()
        ans1$NAVseries <- all_series[[i]]
        ans1$NAV <- NAV
        ans1$timestamp <- timestamp
        ans1$instrument  <- if (!is.null(attr(all_series[[i]], "instrument")))
                                attr(all_series[[i]], "instrument") else NA
        ans1$title       <- if (!is.null(attr(all_series[[i]], "title")))
                                attr(all_series[[i]], "title") else NA
        ans1$description <- if (!is.null(attr(all_series[[i]], "description")) &&
                                length(attr(all_series[[i]], "description")))
                                attr(all_series[[i]], "description") else NA
        ans1$start <- min(timestamp)
        ans1$end   <- max(timestamp)
        ans1$nobs <- nobs
        ans1$nna  <- nna
        ans1$low  <- min(NAV)
        ans1$high <- max(NAV)
        ans1$low.when  <- timestamp[which.min(NAV)[1L]]
        ans1$high.when <- timestamp[which.max(NAV)[1L]]

        ## Returns, annualised
        if (.may_be_Date(c(ans1$start, ans1$end))) {
            t <- as.numeric(as.Date(ans1$end) -
                            as.Date(ans1$start))
        } else if (assume.daily)
            t <- ans1$end - ans1$start
        else
            t <- NA

        if (!is.na(t) && t > 365) {
            ans1$return <- (tail(NAV,1)/head(NAV, 1))^(365/t) - 1
            ans1$return.annualised <- TRUE
        } else {
            ans1$return <- tail(NAV,1)/head(NAV,1) - 1
            ans1$return.annualised <- FALSE
        }

        tmp <- drawdowns(NAV)
        dd.row <- if(nrow(tmp))
                      which.max(tmp[["max"]])
                  else
                      0
        if (dd.row > 0) {
            ans1$mdd      <- tmp[["max"]][dd.row]
            ans1$mdd.high <- NAV[tmp[["peak"]][dd.row]]
            ans1$mdd.low <- NAV[tmp[["trough"]][dd.row]]
            ans1$mdd.high.when <- timestamp[tmp[["peak"]][dd.row]]
            ans1$mdd.low.when <- timestamp[tmp[["trough"]][dd.row]]
            ans1$mdd.recover.when <- timestamp[tmp[["recover"]][dd.row]]
        } else {
            ans1$mdd <- 0
            ans1$mdd.high <- max(NAV)
            ans1$mdd.low <- NA
            ans1$mdd.high.when <- timestamp[which.max(NAV)]
            ans1$mdd.low.when <- NA
            ans1$mdd.recover.when <- NA
        }
        ans1$underwater <- 1 - NAV[length(NAV)]/max(NAV)

        ## Volatility
        if (.may_be_Date(timestamp) && monthly.vol) {
            tmp <- returns(NAV, t = timestamp, period = "month")
            sq12 <- sqrt(12)
            ans1$volatility <- sd(tmp) * sq12
            ans1$volatility.up <- pm(tmp,
                                     normalise = TRUE,
                                     lower = FALSE) * sq12
            ans1$volatility.down <- pm(tmp, normalise = TRUE) * sq12
        } else if (assume.daily) {
            ans1$volatility <- sd(returns(NAV))*16
            ans1$volatility.up <- pm(returns(NAV),
                                     normalise = TRUE,
                                     lower = FALSE)*16
            ans1$volatility.down <- pm(returns(NAV),
                                       normalise = TRUE)*16
        } else {
            ans1$volatility      <- sd(returns(NAV))
            ans1$volatility.up   <- pm(returns(NAV),
                                       normalise = TRUE,
                                       lower = FALSE)
            ans1$volatility.down <- pm(returns(NAV),
                                       normalise = TRUE)
        }

        ## Tracking Error
        if (has.bm) {
            if (i < length(ans)) {
                tmp.NAV <- merge(zoo(NAV, timestamp),
                                 zoo(bm.NAV, bm.timestamp))
                if (.may_be_Date(timestamp) && monthly.te) {
                    r.te <- returns(tmp.NAV, period = "month")
                    r.te <- sd(diff(t(coredata(r.te))), na.rm = na.rm)*sqrt(12)
                } else {
                    r.te <- returns(tmp.NAV)
                    r.te <- sd(diff(t(coredata(r.te))), na.rm = na.rm) *
                        if (assume.daily) 16 else 1
                }
            } else
                r.te <- 0
            ans1$tracking.error <- r.te
        }

        ans[[i]] <- ans1
    }

    class(ans) <- "summary.NAVseries"
    if (has.bm)
        attr(ans, "bm") <- length(ans)
    ans
}

.summary.NAVseries_fields <-
    c("instrument",
      "title",
      "description",
      "start",
      "end",
      "nobs",
      "nna",
      "low.when",
      "high.when",
      "low",
      "high",
      "return",
      "return.annualised",
      "mdd.high.when",
      "mdd.low.when",
      "mdd.high",
      "mdd.low",
      "mdd",
      "mdd.recover.when",
      "underwater",
      "volatility.up",
      "volatility.down",
      "volatility",
      "tracking.error"
      )


.summary.NAVseries_labels <-
    c("Instrument",
      "Title",
      "Description",
      "Start           ",
      "End             ",
      "# observations  ",
      "# missing obs.  ",
      "_ time          ",
      "_ time          ",
      "Low             ",
      "High            ",
      "Return %        ",
      "_ annualised?   ",
      "_ time          ",
      "_ time          ",
      "_ peak          ",
      "_ trough        ",
      "Max. drawdown % ",
      "_ recovery time ",
      "underwater %    ",
      "_ upside        ",
      "_ downside      ",
      "Volatility      ",
      "Tracking error %")
names(.summary.NAVseries_labels) <- .summary.NAVseries_fields


.summary.NAVseries_perc_fields <-
    c("return",
      "mdd",
      "underwater",
      "volatility",
      "volatility.up",
      "volatility.down",
      "tracking.error")

.summary.NAVseries_date_fields <-
    c("start",
      "end",
      "low.when",
      "high.when",
      "mdd.high.when",
      "mdd.low.when",
      "mdd.recover.when")

.summary.NAVseries_price_fields <-
    c("high",
      "low",
      "mdd.high",
      "mdd.low")


print.summary.NAVseries <- function(x, ...,
                                    sparkplot = TRUE,
                                    monthly.returns = TRUE) {
    x.original <- x
    datef <- function(x) {
        if (inherits(x[1L], "Date"))
            x <- format(x, "%d %b %Y")
        else if (inherits(x[1L], "POSIXt"))
            x <- format(x, "%d %b %Y %H:%M")
        x
    }

    percf <- function(x)
        if (!is.null(x))
            format(round(100*x, 1),
                   justify = "right",
                   width = 7,
                   nsmall = 1)

    pricef <- function(x)
        format(x,
               justify = "right",
               width = 7,
               nsmall = 2,
               digits = 2,
               scientific = FALSE)

    if ((lx <- length(x)) == 1L) {

        x <- x[[1L]]

        for (f in .summary.NAVseries_date_fields)
            x[[f]] <- datef(x[[f]]) ## format: dates

        for (f in .summary.NAVseries_perc_fields)
            x[[f]] <- percf(x[[f]]) ## format: %

        for (f in .summary.NAVseries_price_fields)
            x[[f]] <- pricef(x[[f]]) ## format: prices

        line <- paste(rep(.hl.char, 55), collapse = "")
        cat(line, "\n", sep = "")
        print(x$NAVseries, ...)
        template <-
            c(line,
              "Return (%)<>%return%|  (annualised)",
              line,
              "Volatility (%)<>%volatility%|  (annualised)",
              "_ upside<>%volatility.up%|",
              "_ downside<>%volatility.down%|",
              line,
              "High<>%high%|  (%high.when%)",
              "Low<>%low%|  (%low.when%)",
              line,
              "Max. drawdown (%)  <> %mdd%|",
              "_ peak<>%mdd.high%|  (%mdd.high.when%)",
              "_ trough<>%mdd.low%|  (%mdd.low.when%)",
              "_ recovery<>|  (%mdd.recover.when%)",
              "_ underwater now (%)<>%underwater%|",
              line,
              "")
        nx <- names(x)
        for (r in c("NAVseries", "NAV",
                    "timestamp", "title",
                    "description"))
            nx <- nx[nx != r]
        for (n in nx)
            template <- gsub(paste0("%", n, "%"),
                             if (is.null(x[[n]]) || is.na(x[[n]])) "NA" else x[[n]],
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

    } else {
        res <- vector("list", length(.summary.NAVseries_fields))
        names(res) <- .summary.NAVseries_fields

        for (i in seq_along(res)) {
            res.i <- character(lx)
            for (j in seq_len(lx)) {
                field.i <- .summary.NAVseries_fields[i]
                tmp <- x[[j]][[field.i]]

                if (is.null(tmp) || is.na(tmp))
                    tmp <- ""
                else if (field.i %in% .summary.NAVseries_date_fields)
                    tmp <- datef(tmp)
                else if (field.i %in% .summary.NAVseries_perc_fields)
                    tmp <- percf(tmp)
                else if (field.i %in% .summary.NAVseries_price_fields)
                    tmp <- pricef(tmp)
                else
                    format(tmp, justify = "right")

                res.i[j] <- tmp
            }

            ## res.i has length >=2
            ## if (length(unique(res.i)) == 1L)
            ##     res.i[c(FALSE, res.i[-1] == res.i[-lx])] <- ""
            res[[ .summary.NAVseries_fields[i] ]] <- res.i
        }
        tmp <- do.call(rbind, res)

        ##
        H <- character(ncol(tmp))
        j <- tmp["title", ] != ""
        H[j] <- tmp["title", j]

        j <- H == "" & tmp["instrument", ] != ""
        H[j] <- tmp["instrument", j]

        j <- H == ""
        fmt.s <- paste0("%0", floor(log10(ncol(tmp)))+1, "d")
        H[j] <- sprintf(fmt.s, which(j))
        tmp["title", ] <- H

        tmp <- tmp[c(
            "title",
            "start",
            "end",
            "nobs",
            "nna",
            "return",
            "return.annualised",
            "volatility",
            "volatility.up",
            "volatility.down",
            "tracking.error",
            "high",
            "high.when",
            "low",
            "low.when",
            "mdd",
            "mdd.high",
            "mdd.high.when",
            "mdd.low",
            "mdd.low.when",
            "mdd.recover.when",
            "underwater"
        ), ]


        tmp <- cbind(.summary.NAVseries_labels[row.names(tmp)],
                     tmp)
        tmp[1, 1] <- ""  ## remove "Title"
        tmp <- apply(tmp, 2,
                     function(x)
                         format(x, justify = "right"))
        hline <- rep(.hl.char, ncol(tmp)*2 + sum(nchar(tmp[1L, ])))
        for (i in seq_len(nrow(tmp))) {
            cat(tmp[i, ], "\n", sep = "  ")
            if (i %in% c(1, 5, 11, 15))
                cat(hline, "\n", sep = "")
        }
    }

    invisible(x.original)
}


toLatex.summary.NAVseries <-
function(object, ...,
         template = " %title & %return & %volatility & %sparkline \\\\",
         file = NULL,
         include.bm = FALSE) {

    fmt_p <- function(x, ...) {
        if (is.numeric(x))
            format(round(x*100, 1), nsmall = 1)
        else
            x
    }
    ns <- length(object)
    if (!include.bm && !is.null(attr(object, "bm")))
        ns <- ns - 1
    ans <- if (length(template) == 1L && ns > 1L)
               rep(template, ns)
    else
        template

    for (field in .summary.NAVseries_fields) {
        field_values <- unlist(lapply(object, `[[`, field))
        if (is.null(field_values) ||
            length(field_values) == 0L ||
            all(is.na(field_values)))
            field_values <- rep("NA", ns)
        else if (field %in% .summary.NAVseries_perc_fields)
            field_values <- fmt_p(field_values)
        for (i in seq_len(ns))
            ans[i] <- gsub(paste0("%", field), field_values[i], ans[i],
                           fixed = TRUE)
    }

    NAVs <- lapply(object, `[[`, "NAV")
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

plot.NAVseries <- function(x, y = NULL, ...,
                           xlab = "", ylab = "", type = "l") {
    if (!is.null(y))
        stop("scatterplot of *returns* -- not implemented")
    plot(x = attr(x, "timestamp"),
         y = x, type = type,
         xlab = xlab, ylab = ylab, ...)

    invisible()
}

lines.NAVseries <- function(x, ..., type = "l") {
    lines(x = attr(x, "timestamp"),
          y = x, type = type, ...)

    invisible()
}


as.NAVseries <- function(x, ...)
    UseMethod("as.NAVseries")

as.NAVseries.NAVseries <- function(x, ...)
    x

as.NAVseries.zoo <- function(x,
                             instrument = NULL,
                             title = NULL,
                             description = NULL, ...){
    dx <- dim(x)
    if (!is.null(dx) && !any(dx == 1L)) {
        res <- vector("list", dx[2L])
        for (i in seq_along(res)) {
            res[[i]] <-
                NAVseries(NAV = coredata(x[, i]),
                          timestamp = index(x[, i]),
                          instrument = instrument,
                          title = title,
                          description = description)
        }
        res

    } else {
        NAVseries(NAV = coredata(x), timestamp = index(x),
                  instrument = instrument,
                  title = title,
                  description = description)
    }
}

as.NAVseries.btest <- function(x, ...,
                               instrument = NULL,
                               title = NULL,
                               description = NULL,
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
              timestamp = timestamp,
              instrument = instrument,
              title = title,
              description = description)
}

as.zoo.NAVseries <- function(x, ...){
    zoo(x, attr(x, "timestamp"))
}

.summary.NAVseries.template <-
"\\begin{tabular}{lrl}
\\multicolumn{3}{l}{NAV series starts %start%, ends %end%}                       \\\\[-0.25ex]
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
             grepl("^[12][0-9][0-9][0-9]$", trimws(as.character(start))))
        start <- as.Date(paste0(start, "-1-1"))

    if (is.null(end))
        end <- .timestamp(x)[length(.timestamp(x))]
    else if (.may_be_Date(timestamp) &&
             grepl("^[12][0-9][0-9][0-9]$", trimws(as.character(end))))
        end <- as.Date(paste0(end, "-12-31"))

    if (start > end)
        stop(sQuote("start"), " cannot be after ", sQuote("end"))

    i <- which(timestamp >= start)[1L]
    j <- tail(which(timestamp <= end), 1)

    ans <- x[i:j]
    attributes(ans) <- attributes(x)
    .timestamp(ans) <- .timestamp(ans)[i:j]
    ans
}

as.data.frame.summary.NAVseries <- function(x, ...){

    .col <- c("instrument", "title", "description",
              "start", "end", "nobs", "nna",
              "low",
              "high", "low.when", "high.when",
              "return", "return.annualised", "mdd",
              "mdd.high", "mdd.low", "mdd.high.when",
               "mdd.low.when", "mdd.recover.when",
               "underwater", "volatility",
              "volatility.up", "volatility.down")
    .col.labels <-
    c("Instrument",
      "Title",
      "Description",
      "Start",
      "End",
      "n.observations",
      "n.missing",
      "_ time          ",
      "_ time          ",
      "Low             ",
      "High            ",
      "Return %        ",
      "_ annualised?   ",
      "_ time          ",
      "_ time          ",
      "_ peak          ",
      "_ trough        ",
      "Max. drawdown % ",
      "_ recovery time ",
      "underwater %    ",
      "_ upside        ",
      "_ downside      ",
      "Volatility      ",
      "Tracking error %")


    do.call(rbind, x)
}
