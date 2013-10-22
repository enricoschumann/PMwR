journal <- function(timestamp, amount, price, instrument,
                    id = NULL,  account = NULL, ...) {

    ## empty journal
    if (match.call() == "journal()") {
        ans <- list(timestamp = numeric(0),
                    amount = numeric(0),
                    price = numeric(0),
                    instrument = character(0))
        class(ans) <- "journal"
        return(ans)
    }

    ## convert position to journal 
    if (!missing(timestamp) && inherits(timestamp, "position")) {
        if (!missing(price) && !is.null(names(price))) 
            price <- price[match(timestamp$instrument, names(price))]
        else if (missing(price))
            price <- NA
        if (length(timestamp$timestamp) > 1L)
            stop("must be position at one point in time")
        ans <- journal(timestamp  = timestamp$timestamp,
                       amount     = c(timestamp$position),
                       price      = price,
                       instrument = timestamp$instrument)
        return(ans)
    }
        
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

print.journal <- function(x, ..., width = 60L, max.print = 100L,
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
          print.gap=2)
    if (lx > max.print)
        cat("[ ... ]\n\n") else cat("\n")

    subs <- ""
    if (!is.null(x$instruments)) {
        insts <- sort(unique(x$instrument))
        insts <- as.character(rmspace(insts))
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
    ## number of transactions, min/max price, first/last transactions
}

`[.journal`  <- function(x, i, match.against = c("instrument", "account"),
                         ignore.case = TRUE, ..., reverse = FALSE) {
    if (is.character(i)) {
        ii <- logical(length(x))
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
    
    ## TODO
    if (!missing(FUN))
        FUN <- match.fun(FUN)
    j <- journal()
    for (g in sort(unique(grp))) {
        sx <- x[g == grp]
        if (length(sx) == 0L)
            next
        j <- c(j, FUN(sx))
    }
    j
}

split.journal <- function(x, f, drop = FALSE, ...) {
    ## TODO    
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
