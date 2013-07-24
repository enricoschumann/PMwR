journal <- function(timestamp, amount, price, id, instrument, account, ...) {

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
    if (inherits("position", timestamp)) {
        if (!missing(price) && !is.null(names(price))) 
            price <- price[match(timestamp$instrument, names(price))]
        else if (missing(price))
            price <- NA
            ans <- journal(timestamp  = timestamp$timestamp,
                           amount     = timestamp$position,
                           price      = price,
                           instrument = timestamp$instrument)
        returns(ans)
    }
        
    if (missing(id))
        id <- NULL
    if (missing(timestamp))
        timestamp <- NA
    if (missing(instrument) || all(is.na(instrument)))
        instrument <- NA
    else if (all(instrument == instrument[1L]))
        instrument <- instrument[1L]
    if (missing(account))
        account <- NULL
    if (missing(price))
        price <- NA
    instrument <- as.character(instrument)    
    len <- max(length(timestamp),
               length(amount),
               length(price),
               length(id),
               length(instrument),
               length(account))
    rep0 <- function(x, len) {
        if (len == 1L || length(x) == len)
            x
        else 
            rep(x, len)
    }
    ans <- list(id = rep0(id, len),
                instrument = rep0(instrument, len),
                account    = rep0(account, len),
                timestamp  = rep0(timestamp, len),
                amount     = rep0(amount, len),
                price      = rep0(price, len))
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
print.journal <- function(x, ..., width = 60L, max.print = 100,
                          exclude = NULL) {
    oo <- getOption("scipen")
    options(scipen = 1e8)
    on.exit(options(scipen = oo))

    lx <- length(x)
    df <- as.data.frame(unclass(x),                        
                        row.names = seq_len(lx),
                        stringsAsFactors = FALSE)

    ndf <- colnames(df)
    first.cols <- c("instrument", "timestamp", "amount","price")
    df <- df[TRUE, c(first.cols, setdiff(ndf, first.cols)), drop = FALSE]
    
    if (!is.null(exclude))
        df[TRUE, setdiff(ndf, exclude), drop = FALSE]

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
c.journal <- function(...) {
    tls <- list(...)
    if (!all(unlist(lapply(tls, "class")) == "journal"))
        stop("all ... must be journals")
    
    ns <- unique(unlist(lapply(tls, names)))
    ans <- vector("list", length = length(ns))
    names(ans) <- ns
    for (n in seq_along(ns)) {
        ans[[ns[n]]] <- unlist(lapply(tls, `[[`, ns[n]))
    }
    class(ans) <- "journal"
    ans
}
subset.journal <- function(x, ..., account = NULL, instrument = NULL) {
    if (!is.null(account))
        stop("not implemented yet")
    if (!is.null(instrument))
        stop("not implemented yet")
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
as.data.frame.journal <- function(x, row.names = NULL, optional = FALSE, ...) {
    if (!is.null(row.names))
        warning("'row.names' not supported yet")
    if (optional)
        warning("'optional' not supported yet")
    data.frame(unclass(x))
}


## accessors
account <- function(x, pattern = NULL, ...) {

    if (!inherits(x, "journal"))
        stop(sQuote("x"), " must inherit from class ", sQuote("journal"))

    if (is.null(x$account)) {
        NULL
    } else if (!is.null(pattern)) {
        ts <- grep(pattern, x$account, ...)
        x$account[ts]
    }    else
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
summary.journal <- function(x, ...) {
    ## number of trades per instrument
    ## level: instrument or account or factor
    ## number of transactions, min/max price, first/last transactions
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
`[.journal`  <- function(x, i, match.against = NULL) {
    if (is.character(i)) {
        ii <- grepl(i, x$instrument)
        if (!is.null(x$account))
            ii <- ii | grepl(i, x$account)
    }
    ans <- lapply(unclass(x), `[`, ii)
    class(ans) <- "journal"
    ans
}
aggregate.journal <- function(x, by, FUN, ...) {
    if (!missing(FUN))
        FUN <- match.fun(FUN)
    ins <- x$instrument
    sgn <- ifelse(x$amount >= 0, "buy ", "sell")
    grp <- paste(ins, by, sgn, sep = "_")

    j <- journal()
    for (g in sort(unique(grp))) {
        sx <- x[g == grp]
        j <- c(j, journal(amount = sum(sx$amount),
                          price = mean(sx$price),
                          instrument = sx$instrument[1]))
    }
    j
}
