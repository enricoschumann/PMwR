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

journal.position <- function(amount, price, ...) {

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

journal.rebalance <- function(amount, ..., price = TRUE, timestamp = NA) {
    journal(instrument = if (amount$match.names) names(amount$price) else NA,
            timestamp  = timestamp,
            amount = unname(amount$target - amount$current),
            price  = if (isTRUE(price)) unname(amount$price) else NA)
}

journal.btest <- function(amount, ...)
    amount$journal

journal.default <- function(amount, price, timestamp, instrument,
                    id = NULL,  account = NULL, ...) {
        
    if (missing(timestamp))
        timestamp <- NA
    if (missing(instrument) || all(is.na(instrument)))
        instrument <- NA
    else if (all(instrument == instrument[1L]))
        instrument <- instrument[1L]
    if (missing(price))
        price <- NA
    instrument <- as.character(instrument)    
    len <- max(length(timestamp),
               length(amount),
               length(price),
               length(id),
               length(instrument),
               length(account))
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

    dots <- list(...)
    nd <- names(dots)
    if (length(dots)) {
        if (any(nd == "") || is.null(nd))
            stop("arguments passed via ... must be named")
        else
            ans <- c(ans, dots)
    }    
    class(ans) <- "journal"
    ans    
}

print.journal <- function(x, ..., width = getOption("width"),
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
    first.cols <- c("instrument", "timestamp", "amount","price")
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
        insts <- as.character(trim(insts))
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
    o <- order(x[[by]], na.last = na.last, decreasing = decreasing)    
    for (i in seq_along(unclass(x)))
        x[[i]]<- x[[i]][o]
    x    
}

c.journal <- function(..., recursive = FALSE) {
    tls <- list(...)
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
    classes[unlist(lapply(tls, length)) == 0L] <- NULL ## remove empty journals
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

`account<-` <- function(x, value) {
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

amount <- function(x, abs = FALSE, ...) {
    if (!inherits(x, "journal"))
        stop(sQuote("x"), " must inherit from class ", sQuote("journal"))
    x$amount
}

`amount<-` <- function(x, value) {

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

summary.journal <- function(x, ...) {
    ## TODO
    ## number of trades per instrument
    ## level: instrument or account or factor
    ## number of transactions, first/last transactions, min/max price (if meaningful) 
    ans <- list()
    ans$n_transactions <- length(x)
    ans
}

print.summary.journal <- function(x, ...) {
    cat("Journal with ", x$n_transactions, " transactions.")
    invisible(x)
}


`[.journal`  <- function(x, i, match.against = NULL,
                         ignore.case = TRUE, ..., reverse = FALSE) {
    if (is.character(i)) {
        if (is.null(match.against))
            match.against  <- names(x)[unlist(lapply(x, mode)) == "character"]
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

aggregate.journal <- function(x, by, FUN, ...) {

    lenx <- length(x)    
    grp <- double(lenx)
    for (ind in rev(by)) {
        if (length(ind) != lenx) 
            stop("all vectors in 'by' must have same length")
        ind <- as.factor(ind)
        grp <- grp * nlevels(ind) + (as.integer(ind) - 1L)
    }
    
    if (mode(FUN) ==  "list") {
        funlist <- FUN
        FUN <- function(x, ...) {
            ans <- list()
            for (i in seq_along(funlist))
                ans <- c(ans, funlist[[i]](x[[ names(funlist)[i] ]], ...))
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
        x
    if (by) {
        insts <- sort(unique(x$instrument))
        ans <- journal()
        for (i in insts) {
            sx <- x[x$instrument == i]
            ans <- c(ans, sx[seq_len(min(n, length(sx)))])            
        }
        ans
    } else {
        x[seq_len(min(n, lenx))]        
    }
}

tail.journal <- function(x, n = 6L, ..., by = TRUE) {
    if ((lenx <- length(x)) <= 1L)
        x
    if (by) {
        insts <- sort(unique(x$instrument))
        ans <- journal()
        for (i in insts) {
            sx <- x[x$instrument == i]
            if (length(sx == 0L))
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

is.journal <- function (x) 
    inherits(x, "journal")

as.journal <- function(x, ...)
    UseMethod("as.journal")

as.journal.data.frame <- function(x, ...)
    do.call("journal", as.list(x))

str.journal <- function(object, ...) {
    n <- length(object)
    cat(sQuote("journal"), ":\t ",
        n, " transaction", if (n != 1) "s", "\n",
        sep = "")
    tmp <- capture.output(str(unclass(object), ...))
    cat(paste(tmp[-1], collapse = "\n", sep = ""), "\n")
    invisible()
}
