## -*- truncate-lines: t; -*-
## Copyright (C) 2008-19  Enrico Schumann

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
        timestamp <- NA
    if (missing(instrument) || all(is.na(instrument)))
        instrument <- NA
    else if (!any(is.na(instrument)) &&
             all(instrument == instrument[1L]))
        instrument <- instrument[1L]
    if (missing(price))
        price <- NA
    instrument <- as.character(instrument)


    len <- max(length(timestamp),
               length(amount),
               length(price),
               length(id),
               length(instrument),
               length(account),
               if (length(dots)) max(lengths(dots)))

    ans <- list(id         = rep(id,         len/length(id)),
                instrument = rep(instrument, len/length(instrument)),
                account    = rep(account,    len/length(account)),
                timestamp  = rep(timestamp,  len/length(timestamp)),
                amount     = rep(amount,     len/length(amount)),
                price      = rep(price,      len/length(price)))

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

summary.journal <- function(object, ...) {
    ans <- list()
    ans$n_transactions <- length(object)
    if (ans$n_transactions == 0L) {
        ans$stats <- NA
        class(ans) <- "summary.journal"
        return(ans)
    }

    ## TODO aggregation level? instrument or account or factor ...
    no_instrument <- FALSE
    if (all(is.na(xi <- instrument(object)))) {
        xi <- object$instrument <- rep("_", length(object))
        no_instrument <- TRUE
    }

    stats <- data.frame(instrument = character(0),
                        n_transactions = numeric(0),
                        average_buy = numeric(0),
                        average_sell = numeric(0),
                        first_t = numeric(0),
                        last_t = numeric(0),
                        stringsAsFactors = FALSE)
    for (i in sort(unique(xi))) {
        ji <- object[ i==xi ]
        si <- data.frame(
            instrument   = i,
            n_t          = length(ji),
            average_buy  = mean(ji$price[ji$amount > 0], na.rm=TRUE),
            average_sell = mean(ji$price[ji$amount < 0], na.rm=TRUE),
            first_t      = min(ji$timestamp)[[1L]],
            last_t       = max(ji$timestamp)[[1L]],
            stringsAsFactors = FALSE)
        stats <- rbind(stats, si)
    }
    if (no_instrument)
        stats$instrument <- NA
    ans$stats  <- stats
    class(ans) <- "summary.journal"
    ans
}

print.summary.journal <- function(x, ...) {
    if (x$n_transactions > 0L) {
        ans <- x$stats
        has_instrument <- !all(is.na(x$stats$instrument))
        if (!has_instrument)
            ans$instrument <- NULL
        cat("journal: ", x$n_transactions, " transactions ",
            "in ", nrow(ans), " instrument",
            if (nrow(ans) > 1L) "s", "\n\n", sep = "")
        colnames(ans) <- c(if (has_instrument) "instrument",
                           "n", "avg buy", "avg sell",
                           "first", "last")
        for (i in 3:4)
            ans[[i]][!is.finite(ans[[i]])] <- NA

        if (has_instrument)
            ans[["instrument"]] <-
                paste0("",
                       format(ans[["instrument"]], justify = "left"))
        print.data.frame(ans, na.print = "",
                         print.gap = 2, row.names = FALSE)
    } else {
        cat("no transactions\n")
    }
    invisible(x)
}

`[.journal`  <- function(x, i, match.against = NULL,
                         ignore.case = TRUE, ...,
                         reverse = FALSE) {
    if (is.character(i)) {
        if (is.null(match.against))
            match.against <- names(x)[unlist(lapply(x, mode)) == "character"]
        ii <- logical(length(x))
        if (length(i) > 1L)
            i <- paste(i, collapse = "|")
        for (m in match.against) {
            if (is.null(x[[m]]))
                next
            ii <- ii | grepl(i, x[[m]], ignore.case = ignore.case, ...)
        }
        if (reverse)
            ii <- !ii
    } else
        ii <- i
    ans <- lapply(unclass(x), `[`, ii)
    class(ans) <- "journal"
    ans
}

`[<-.journal`  <- function(x, i, match.against = NULL,
                           ignore.case = TRUE, ..., reverse = FALSE, value) {

    stop("extraction only: use x$amount[] etc. for replacement")
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

head.journal <- function(x, n = 6L, ..., by = TRUE) {
    if ((lenx <- length(x)) <= 1L)
        return(x)
    x <- sort(x)
    if (by) {
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

tail.journal <- function(x, n = 6L, ..., by = TRUE) {
    if ((lenx <- length(x)) <= 1L)
        return(x)
    x <- sort(x, decreasing = TRUE)
    if (by) {
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

cashflows <- function(x, multiplier = 1, ...) {

    if (!is.null(names(multiplier)))
        multiplier <- multiplier[x$instrument]
    ans <- x
    ans$instrument <- "cash"
    ans$amount <- -x$amount * x$price * multiplier
    ans$price <- 1
    ans
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
