## -*- truncate-lines: t; -*-
## Copyright (C) 2008-20  Enrico Schumann

journal <- function(amount, ...) {
    if (match.call() == "journal()") {
        ans <- list(timestamp = numeric(0),
                    amount = numeric(0),
                    price = numeric(0),
                    instrument = character(0))
        class(ans) <- "journal"
        ans
    } else
        UseMethod("journal")
}

journal.position <- function(x, price, ...) {
    warning("did you mean 'as.journal'?")
    journal.default(amount = x, price = price, ...)
}

journal.default <- function(amount, price, timestamp, instrument,
                            id = NULL,  account = NULL, ...) {

    dots <- list(...)
    nd <- names(dots)

    if (!length(amount)) {
        ## TODO: copy ... fields from journal

        ## TODO: add warning if there
        ##       are non--zero-length fields
        ans <- list(timestamp = numeric(0),
                    amount = numeric(0),
                    price = numeric(0),
                    instrument = character(0))
        class(ans) <- "journal"
        return(ans)
    }

    if (missing(timestamp))
        timestamp <- NA_real_
    if (missing(instrument) || all(is.na(instrument)))
        instrument <- NA_character_
    else if (!any(is.na(instrument)) &&
             all(instrument == instrument[1L]))
        instrument <- instrument[1L]
    if (missing(price))
        price <- NA_real_
    instrument <- as.character(instrument)


    len <- max(length(timestamp),
               length(amount),
               length(price),
               length(id),
               length(instrument),
               length(account),
               if (length(dots)) max(lengths(dots)))

    ans <- list(
        id         = if ((n <- len/length(id)) == 1)         id else rep(id, n),
        instrument = if ((n <- len/length(instrument)) == 1) instrument else rep(instrument, n),
        account    = if ((n <- len/length(account)) == 1)    account else rep(account, n),
        timestamp  = if ((n <- len/length(timestamp)) == 1)  timestamp else rep(timestamp, n),
        amount     = if ((n <- len/length(amount)) == 1)     amount else rep(amount, n),
        price      = if ((n <- len/length(price)) == 1)      price else rep(price, n))

    ## remove NULL
    isNul <- unlist(lapply(ans, is.null))
    for (i in seq_along(isNul))
        if (isNul[i])
            ans[[names(isNul[i])]] <- NULL

    if (length(dots)) {
        if (any(nd == "") || is.null(nd))
            stop("arguments passed via ... must be named")
        else {
            ldots <- lengths(dots)
            bad_len <- ldots != len
            if (any(bad_len)) {
                for (i in which(bad_len))
                    dots[[i]] <- rep(dots[[i]], len/ldots[[i]])
            }
            ans <- c(ans, dots)
        }
    }
    class(ans) <- "journal"
    ans
}

journal.btest <- function(amount, ...)
    amount$journal

print.journal <- function(x, ...,
                          width = getOption("width"),
                          max.print = getOption("max.print"),
                          exclude = NULL, include.only = NULL) {
    lx <- length(x)
    if (lx == 0L) {
        cat("no transactions\n")
        return(invisible(x))
    }
    oo <- getOption("scipen")
    options(scipen = 1e8)
    on.exit(options(scipen = oo))

    df <- as.data.frame(unclass(x),
                        row.names = seq_len(lx),
                        stringsAsFactors = FALSE)

    ndf <- colnames(df)
    first.cols <- c("instrument", "timestamp", "amount", "price")
    df <- df[TRUE, c(first.cols, setdiff(ndf, first.cols)), drop = FALSE]

    if (!is.null(exclude))
        df <- df[TRUE, setdiff(ndf, exclude), drop = FALSE]

    notAllNA <- unlist(lapply(df, function(x) !all(is.na(x))))
    print(head(df[notAllNA], max.print), quote = FALSE,
          print.gap = 2)
    if (lx > max.print)
        cat("[ .... ]\n\n") else cat("\n")

    subs <- ""
    if (!is.null(x$instruments)) {
        insts <- sort(unique(x$instrument))
        insts <- as.character(trimws(insts))
        if (length(insts))
            subs <- paste0(" in ", paste(insts, sep = "", collapse = ", "))
    }
    if (lx > 1L || lx == 0L)
        ps <- "s" else ps <- ""

    msg <- strwrap(paste0("\n", lx, " transaction", ps, subs), width)
    msg <- paste(msg[1L], if (length(msg)>1L) "...", "\n")
    cat(msg)
    invisible(x)
}

length.journal <- function(x)
    length(x$amount)

sort.journal <- function(x, decreasing = FALSE, by = "timestamp",
                         ..., na.last = TRUE) {
    o <- do.call(order,
                 c(unclass(x)[by],
                   na.last = na.last, decreasing = decreasing))
    for (i in seq_along(unclass(x)))
        x[[i]]<- x[[i]][o]
    x
}

c.journal <- function(..., recursive = FALSE) {
    tls <- list(...)
    tls <- tls[!unlist(lapply(tls, is.null))]
    if (!all(unlist(lapply(tls, inherits, "journal")) ))
        warning("method only works for ", sQuote("journal"), " objects")
    ns <- unique(unlist(lapply(tls, names)))
    ans <- vector("list", length = length(ns))
    names(ans) <- ns
    for (n in seq_along(ns)) {
        nul <- unlist(lapply(lapply(tls, `[[`, ns[n]), is.null))
        for (i in which(nul))
            tls[[i]][[ns[n]]] <- rep(NA, length(tls[[i]]))
        ans[[ns[n]]] <- unlist(lapply(tls, `[[`, ns[n]))
    }

    nts <- which(ns == "timestamp")
    classes <- lapply(lapply(tls, `[[`, "timestamp"), class)
    if (any(lengths(tls)))
        classes[lengths(tls) == 0L] <- NULL ## remove empty journals
    class(ans[[nts]]) <- classes[[1L]]
    if (length(unique(lapply(classes, paste, collapse = ""))) > 1L)
        warning("different timestamp classes")
    class(ans) <- "journal"
    ans
}

subset.journal <- function(x, ...) {
    i <- with(x, ...)
    ans <- lapply(unclass(x), `[`, i)
    class(ans) <- "journal"
    ans
}

joinAI <- function(x, sep = "::") {
    tmp <- paste0(x$account, sep, x$instrument)
    x$account <- NA
    x$instrument <- tmp
    x
}

as.data.frame.journal <- function(x, row.names = NULL,
                                  optional = FALSE, ...)
    as.data.frame(unclass(x),
                  row.names = row.names, optional = optional, ...)

## accessors
account <- function(x, ...) {
    if (!inherits(x, "journal"))
        stop(sQuote("x"), " must inherit from class ", sQuote("journal"))

    if (is.null(x$account))
        NULL
    else
        x$account
}

`account<-` <- function(x, ..., value) {
    if (!inherits(x, "journal"))
        stop(sQuote("x"), " must inherit from class ", sQuote("journal"))

    len <- length(value)
    lenx <- length(x)
    if (len == lenx)
        x$account <- value
    else if (len == 1L)
        x$account <- rep(value, lenx)
    x
}

amount <- function(x, ...) {
    if (!inherits(x, "journal"))
        stop(sQuote("x"), " must inherit from class ", sQuote("journal"))
    x$amount
}

`amount<-` <- function(x, ..., value) {

    if (!inherits(x, "journal"))
        stop(sQuote("x"), " must inherit from class ", sQuote("journal"))

    len <- length(value)
    lenx <- length(x)
    if (len == lenx)
        x$amount <- value
    else if (len == 1L)
        x$amount <- rep(value, lenx)
    x
}

instrument <- function(x, ...) {
    UseMethod("instrument")
}

`instrument<-` <- function(x, ..., value) {
    UseMethod("instrument<-")
}

instrument.position <- function(x, ...) {
    attr(x, "instrument")
}

instrument.pricetable <- function(x, ...) {
    attr(x, "instrument")
}

instrument.journal <- function(x, ...) {
    x$instrument
}

`instrument<-.journal` <- function(x, ..., value) {
    len <- length(value)
    lenx <- length(x)
    if (len == lenx)
        x$instrument <- value
    else if (len == 1L)
        x$instrument <- rep(value, lenx)
    else
        stop("length(instrument) differs from length(journal)")
    x

}

summary.journal <- function(object,
                            by = "instrument",
                            drop.zero = TRUE,
                            na.rm = FALSE,
                            ...) {

    if (length(object) == 0L) {
        ans <- structure(list(n = numeric(0),
                              average_buy = numeric(0),
                              average_sell = numeric(0),
                              turnover = numeric(0),
                              first_t = numeric(0),
                              last_t = numeric(0)),
                         class = "data.frame",
                         row.names = integer(0))
        class(ans) <- c("summary.journal", "data.frame")
        return(ans)
    }

    no_instrument <- FALSE
    if (all(is.na(instrument(object)))) {
        object$instrument <- rep("___", length(object))
        no_instrument <- TRUE
    }


    ## TODO aggregation level? instrument or account or factor ...
    INDEX <- NULL
    INDEX.names <- NULL
    if (!is.list(by)) {
        ## TODO date timestamp month hour quarter day
        if ("instrument" %in% by) {
            INDEX <- c(INDEX, instrument = list(object$instrument))
            INDEX.names <- c(INDEX.names, "instrument")
        }
        if ("year" %in% by) {
            INDEX <- c(INDEX, year = list(format(object$timestamp, "%Y")))
            INDEX.names <- c(INDEX.names, "year")
        }
        if ("month" %in% by) {
            INDEX <- c(INDEX, month = list(format(object$timestamp, "%m")))
            INDEX.names <- c(INDEX.names, "month")
        }
        if ("date" %in% by) {
            INDEX <- c(INDEX, date = as.Date(object$timestamp))
            INDEX.names <- c(INDEX.names, "month")
        }
    } else {
        INDEX <- by
        INDEX.names <- names(by)
    }

    ## if (isTRUE(by.timestamp)) {
    ##     by.timestamp <- object$timestamp
    ##     tmp <- .may_be_Date(by.timestamp)
    ##     if (tmp) {
    ##         tmp <- attr(tmp, "Date")
    ##         INDEX <- c(INDEX,
    ##                    list(year = datetimeutils::year(tmp),
    ##                         month = datetimeutils::month(tmp)))
    ##     }
    ## }

    ## if (isTRUE(by.instrument))
    ##     INDEX <- c(INDEX,
    ##                instrument = list(object$instrument))


    ## if (is.null(INDEX))
    ##     INDEX <- rep(1, ans$n_transactions)

    stats <- as.data.frame(table(INDEX),
                           responseName = "n_transactions0",
                           stringsAsFactors = FALSE)
    ## if ("month" %in% colnames(stats) &&
    ##     is.character(stats[["month"]]) &&
    ##     all(stats[["month"]] %in% as.character(1:12)))
    ##     stats[["month"]] <- as.numeric(stats[["month"]])
    ## if ("year" %in% colnames(stats) &&
    ##     is.character(stats[["year"]]) &&
    ##     all(stats[["year"]] %in% as.character(1:12)))
    ##     stats[["year"]] <- as.numeric(stats[["year"]])


    stats <- data.frame(stats,
                        n = as.matrix(
                            c(tapply(object$amount, INDEX = INDEX,
                                     length, default = 0))),
                        average_buy = as.matrix(
                            c(tapply(
                                object, INDEX = INDEX,
                                function(x)
                                sum(x$price[x$amount > 0] * x$amount[x$amount > 0], na.rm = na.rm) / sum(x$amount[x$amount > 0])))),
                        average_sell = as.matrix(
                            c(tapply(
                                object, INDEX = INDEX,
                                function(x)
                                sum(x$price[x$amount < 0] * x$amount[x$amount < 0], na.rm = na.rm) / sum(x$amount[x$amount < 0])))),
                        turnover = as.matrix(
                            c(tapply(
                                object, INDEX = INDEX,
                                function(x)
                                sum(x$price * abs(x$amount))))),
                        first_t = as.matrix(
                            c(tapply(
                                object$timestamp, INDEX = INDEX, min))),
                        last_t = as.matrix(
                            c(tapply(
                                object$timestamp, INDEX = INDEX, min))),
                        stringsAsFactors = FALSE)

    stats$n_transactions0 <- NULL

    if (!is.null(stats[["first_t"]]))
        class(stats[["first_t"]]) <-
            class(stats[["last_t"]]) <- class(object$timestamp)

    if (drop.zero)
        stats <- stats[stats$n > 0L, ]

    if (length(INDEX) == 1L && !is.null(INDEX.names))
        colnames(stats)[colnames(stats) == "INDEX"] <- INDEX.names

    if (no_instrument)
        stats$instrument <- NA

    ## sort
    if (!is.null(by))
        for (b in INDEX.names)
            if (!is.null(stats[[b]]))
                stats <- stats[order(stats[[b]]), ]

    class(stats) <- c("summary.journal", "data.frame")
    stats
}

.repeated <- function(x, ...)
    c(FALSE, x[-1L] == x[-length(x)])

print.summary.journal <- function(x, month.names = month.abb,
                                  digits = NULL, na.print = "",
                                  use.crayon = NULL, ...) {
    if (nrow(x) > 0L) {
        stats <- x
        has_instrument <- !all(is.na(stats$instrument))
        if (!has_instrument)
            stats$instrument <- NULL
        msg <- c("journal: ", sum(x$n), " transactions ")
        if (has_instrument) {
            if ( (ni <- length(unique(x$instrument))) != 1L )
                msg <- c(msg, "in ", ni, " instruments")
            else
                msg <- c(msg, "in 1 instrument")
            }
        cat(msg, "\n\n", sep = "")
        ex <- colnames(stats) %in% c("first_t", "last_t")
        stats <- stats[, !ex]


        if ("year" %in% colnames(stats))
            stats[["year"]][ .repeated(stats[["year"]]) ] <- ""

        if ("month" %in% colnames(stats) &&
            !.isFALSE(month.names) &&
            is.numeric(stats[["month"]]))
            stats[["month"]][] <- month.names[ stats[["month"]] ]

        if ("month" %in% colnames(stats))
            stats[["month"]][ .repeated(stats[["month"]]) ] <- ""

        pretty.colnames <- c("n_transactions" = "n",
                             "average_buy"  = "avg buy",
                             "average_sell" = "avg sell")

        tmp <- pretty.colnames[colnames(stats)]
        colnames(stats)[!is.na(tmp)] <- tmp[!is.na(tmp)]

        is.char <- which(unlist(lapply(stats, is.character)))
        ans <- rbind(stats[1, ], stats)
        for (j in seq_len(ncol(stats))) {
            na <- if (is.numeric(stats[[j]]))
                      !is.finite(stats[[j]])
                  else
                      is.na(stats[[j]])
            na <- c(FALSE, na)
            if (j %in% is.char) {
                tmp <- trimws(c(colnames(stats)[j], stats[[j]]))
                tmp[na] <- na.print
                ans[[j]] <- I(format(tmp,
                                   width = max(nchar(tmp)) +1,
                                   justify = "left"))
            } else {
                tmp <- trimws(c(colnames(stats)[j],
                                format(stats[[j]], digits = digits, ...)))
                tmp[na] <- na.print
                ans[[j]] <- I(format(tmp,
                                   width = max(nchar(tmp)) +1,
                                   justify = "right"))
            }
        }

        write.table(ans,
                    row.names = FALSE,
                    col.names  = FALSE,
                    quote = FALSE)
    } else {
        cat("no transactions\n")
    }
    invisible(x)
}

`[.journal`  <- function(x, i, match.against = NULL,
                         ignore.case = TRUE,
                         perl = FALSE,
                         fixed = FALSE,
                         useBytes = FALSE, ...,
                         invert = FALSE) {
    if (match("reverse", names(list(...)), nomatch = 0L)) {
        message(sQuote("reverse"), " is deprecated ==> use ",
                sQuote("invert"), " instead")
        invert <- list(...)$reverse
    }
    if (is.character(i)) {
        if (is.null(match.against))
            match.against <- names(x)[unlist(lapply(x, mode)) == "character"]
        ii <- logical(length(x))
        if (length(i) > 1L)
            i <- paste(i, collapse = "|")
        for (m in match.against) {
            if (is.null(x[[m]]))
                next
            ii <- ii | grepl(i, x[[m]],
                             ignore.case = ignore.case,
                             perl = perl,
                             fixed = fixed,
                             useBytes = useBytes)
        }
    } else
        ii <- i

    if (invert) {
        if (is.numeric(ii)) {
            ii_ <- !logical(length(x))
            ii_[ii] <- FALSE
            ii <- ii_
        } else
            ii <- !ii
    }
    ans <- lapply(unclass(x), `[`, ii)
    class(ans) <- "journal"
    ans
}

`[<-.journal`  <- function(x, i, match.against = NULL,
                           ignore.case = TRUE, ..., invert = FALSE, value) {

    stop("extraction only ==> use x$amount[] etc. for replacement")
}

aggregate.journal <- function(x, by, FUN, ...) {

    lenx <- length(x)
    grp <- double(lenx)
    if (is.atomic(by))
        by <- list(by)
    for (ind in rev(by)) {
        if (length(ind) != lenx)
            stop("all vectors in ", sQuote("by"),
                 " must have same length")
        ind <- as.factor(ind)
        grp <- grp * nlevels(ind) + (as.integer(ind) - 1L)
    }

    if (mode(FUN) ==  "list") {
        funlist <- FUN
        FUN <- function(x, ...) {
            ans <- list()
            for (i in seq_along(funlist))
                ans <- c(ans, funlist[[i]](x[[ names(funlist)[i] ]], ...))
            names(ans) <- names(funlist)
            class(ans) <- "journal"
            ans
        }
    }

    j <- journal()
    for (g in sort(unique(grp))) {
        sx <- x[g == grp]
        if (length(sx) == 0L)
            next
        j <- c(j, FUN(sx, ...))
    }
    j
}

split.journal <- function(x, f, drop = FALSE, ...) {
    lapply(split(x = seq_len(length(x)), f = f, drop = drop, ...),
           function(ind) x[ind])
}



## ================= [ head, tail ] =================

head.journal <- function(x, n = 6L, ..., by = "instrument") {
    if (is.null(by))
        by <- FALSE
    if (isTRUE(by))
        by <- "instrument"
    if ((lenx <- length(x)) <= 1L)
        return(x)
    x <- sort(x)
    if (by == "instrument") {
        insts <- sort(unique(x$instrument))
        ans <- journal()
        for (i in insts) {
            sx <- x[x$instrument == i]
            if (length(sx) == 0L)
                next
            ans <- c(ans, sx[seq_len(min(n, length(sx)))])
        }
        ans
    } else {
        x[seq_len(min(n, lenx))]
    }
}

tail.journal <- function(x, n = 6L, ..., by = "instrument") {
    if (is.null(by))
        by <- FALSE
    if (isTRUE(by))
        by <- "instrument"
    if ((lenx <- length(x)) <= 1L)
        return(x)
    x <- sort(x, decreasing = TRUE)
    if (by == "instrument") {
        insts <- sort(unique(x$instrument))
        ans <- journal()
        for (i in insts) {
            sx <- x[x$instrument == i]
            if (length(sx) == 0L)
                next
            ans <- c(ans, sx[seq_len(min(n, length(sx)))])
        }
        ans
    } else {
        x[(lenx - min(n, lenx) + 1L):lenx]
    }
}



## ================= [ is.journal ] =================

is.journal <- function (x)
    inherits(x, "journal")



## ================= [ as.journal ] =================

as.journal <- function(x, ...)
    UseMethod("as.journal")

as.journal.default <- function(x, ...) {
    if (is.vector(x) && is.numeric(x))
        if (is.null(names(x)))
            journal.default(amount = x)
        else
            journal.default(amount = unname(x),
                            instrument = names(x))
    else
        stop("no default method")
}

as.journal.list <- function(x, ...)
    do.call("journal", x)

as.journal.data.frame <- function(x, ...) {
    lx <- as.list(x)
    if (!nrow(x)) {
        class(lx) <- "journal"
        lx
    } else
        do.call("journal", lx)
}

as.journal.position <- function(x, price, ...) {
    amount <- x
    if (!missing(price) && !is.null(names(price)))
        price <- price[match(attr(amount, "instrument"), names(price))]
    else if (missing(price))
        price <- NA
    if (length(attr(amount, "timestamp")) > 1L)
        stop("must be position at *one* point in time")

    journal(timestamp  = attr(amount, "timestamp"),
            amount     = c(amount),
            price      = price,
            instrument = attr(amount, "instrument"))
}

as.journal.rebalance <- function(x, ..., price = TRUE, timestamp = NA,
                                 drop.zero = TRUE) {
    amount <- x
    mn <- attr(amount, "match.names")
    if (drop.zero)
        amount <- amount[amount[["target"]] != 0 , , drop = FALSE]
    journal(instrument = if (mn)
                             amount[["instrument"]]
                         else
                             NA,
            timestamp  = timestamp,
            amount     = unname(amount[["difference"]]),
            price      = if (isTRUE(price))
                             unname(amount[["price"]])
                         else
                             NA)
}


## =========== [ methods for other generics ] ===========

str.journal <- function(object, ...) {
    n <- length(object)
    cat(sQuote("journal"), ":\t ",
        n, " transaction", if (n != 1) "s", "\n",
        sep = "")
    tmp <- capture.output(str(unclass(object), ...))
    cat(paste(tmp[-1], collapse = "\n", sep = ""), "\n")
    invisible()
}

toOrg.journal <- function(x, inactive = TRUE, ...) {
    df <- as.data.frame.journal(x)
    if (inherits(df[["timestamp"]], "Date"))
        df[["timestamp"]] <- toOrg(df[["timestamp"]],
                                   inactive = inactive)
    toOrg(df)
}

all.equal.journal <- function(target, current,
                              ignore.sort = TRUE, ...) {

    if (!(inherits(current, "journal")))
        stop(sQuote("current"), " must be a journal")

    if (ignore.sort) {
        ## TODO: may fail with duplicate timestamps
        default <- c("timestamp", "instrument", "price", "amount")
        t.f <- names(target)
        t.f <- sort(setdiff(t.f, default))
        t.c <- names(current)
        t.c <- sort(setdiff(t.c, default))

        target <- sort(target, by = c(default, t.f))
        current <- sort(current, by = c(default, t.c))
    }

    msg <- NULL

    ## LENGTH
    t.len <- length(target)
    c.len <- length(current)
    if (t.len != c.len) {
        msg <- c(msg,
                 paste0("lengths differ: target ", t.len,
                        ", current ", c.len))
    }

    ## FIELDS
    t.names <- sort(names(target))
    c.names <- sort(names(current))
    if (length(t.names) != length(c.names) ||
        !all(t.names == c.names)) {

        t.only <- setdiff(t.names, c.names)
        c.only <- setdiff(c.names, t.names)

        if (length(t.only))
            t.only <- paste(sQuote(t.only), collapse = ", ")
        if (length(c.only))
            c.only <- paste(sQuote(c.only), collapse = ", ")
        msg <- c(msg,
                 paste0("fields differ: ",
                        if (length(t.only))
                            paste0("target has ", t.only),
                        if (length(t.only) && length(c.only))
                            "; ",
                        if (length(c.only))
                            paste0("current has ", c.only)))
    }

    ## CONTENTS
    for (f in intersect(t.names, c.names)) {
        tmp <- all.equal(target[[f]], current[[f]])
        if (!isTRUE(tmp))
            msg <- c(msg, paste0(f, ": ", tmp))
    }


    if (is.null(msg))
        TRUE
    else
        msg

}


cashflows <- function(x, multiplier = 1, ...) {

    if (!is.null(names(multiplier)))
        multiplier <- multiplier[x$instrument]
    ans <- x
    ans$instrument <- "cash"
    ans$amount <- -x$amount * x$price * multiplier
    ans$price <- 1
    ans
}
