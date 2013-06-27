journal <- function(timestamp, amount, price, id, instrument, account, ...) {
    if (missing(id))
        id <- NULL
    if (missing(timestamp))
        timestamp <- NA
    if (missing(instrument))
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
print.journal <- function(x, ..., width = 60L) {
    oo <- getOption("scipen")
    options(scipen = 1e8)
    on.exit(options(scipen = oo))

    dspT <- 10 ## display trades, instruments

    
    ## if (!is.null(x$account) && all(!is.na(x$account)))
    ##     rn <- x$account        
    ## if (!is.null(x$id) && all(!is.na(x$id)))
    ##     rn <- paste(rn, x$id, sep = " | ")

    df <- as.data.frame(unclass(x),                        
                        row.names = seq_len(length(x)),
                        stringsAsFactors = FALSE)
    notAllNA <- unlist(lapply(df, function(x) !all(is.na(x))))
    print(head(df[notAllNA],dspT), quote = FALSE,
          print.gap=2)
    if ((n <- max(length(x$amount),length(x$price))) > dspT)
        cat("[ ... ]\n\n") else cat("\n")
    insts <- sort(unique(x$instrument))
    insts <- as.character(rmspace(insts))
    if (length(insts))
        subs <- paste0(" in ", paste(insts, sep = "", collapse = ", "))
    else
        subs <- ""
    msg <- strwrap(paste0("\n", n, " transactions", subs), width)
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
subset.journal <- function(x, ...) {
    i <- with(x, ...)
    ans <- lapply(unclass(x), `[`, i)
    class(ans) <- "journal"
    ans
}
joinAI <- function(x, sep = "::") {
    tmp <- paste0(x$account, sep, $instrument)
    x$account <- NA
    x$instrument <- tmp
    x
}
as.data.frame.journal <- function(x, row.names = NULL, optional = FALSE, ...) {
    if (!is.null(row.names))
        warning("'row.names' not supported yet")
    if (!is.null(optional))
        warning("'optional' not supported yet")
    data.frame(unclass(x))
}
