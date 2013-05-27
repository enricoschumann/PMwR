Journal <- function(timestamp, amount, price, id, instrument, account, ...) {
    ## timestamp, amount, price
    
    if (missing(id))
        id <- NA
    if (missing(instrument))
        instrument <- NA
    else if (all(instrument == instrument[1L]))
        instrument <- instrument[1L]
    if (missing(account))
        account <- NA
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
    dots <- list(...)
    nd <- names(dots)
    if (length(dots)) {
        if (any(nd == "") || is.null(nd))
            stop("arguments passed via ... must be named")
        else
            ans <- c(ans, dots)
    }    
    class(ans) <- "Journal"
    ans    
}
print.Journal <- function(x, ...) {
    oo <- getOption("scipen")
    options(scipen = 1e8)
    on.exit(options(scipen = oo))

    dspT <- 10 ## display trades, instruments
    dspI <- 8
    
    if (all(!is.na(x$account)))
        rn <- x$account        
    if (all(!is.na(x$id)))
        rn <- paste(rn, x$id, sep = " | ")

    df <- as.data.frame(unclass(x),                        
                        row.names = seq_len(length(x)),
                        stringsAsFactors = FALSE)
    notAllNA <- unlist(lapply(df, function(x) !all(is.na(x))))
    print(head(df[notAllNA],dspT))
    if ((n <- max(length(x$amount),length(x$price))) > dspT)
        cat("...\n")
    insts <- sort(unique(x$instrument))
    if (length(insts) > dspI) {
        insts <- insts[seq_len(dspI)]
        insts <- as.character(rmspace(insts))
        insts[dspI] <- "..."
    }
    if (length(insts))
        subs <- paste0(" in ", paste(insts, sep = "", collapse = ", "))
    else
        subs <- ""
    cat(paste0("\n", n, " trades", subs, "\n"))
    invisible(x)
}
length.Journal <- function(x)
    length(x$amount)
sort.Journal <- function(x, ...) {
    cat("not implemented\n")
    invisible(x)
}
filterJournal <- function(x, timestamp, amount, price,
                            id, instrument, account, ...) {    
    cat("not implemented\n")
    invisible(x)
}
## c.Journal0 <- function(...) {
##     tls <- list(...)
##     if (length(tls) > 1L) {
##         ans <- as.data.frame(unclass(tls[[1L]]), stringsAsFactors = FALSE)
##         for (i in 2:length(tls))
##             ans <- rbind(ans, as.data.frame(unclass(tls[[i]]),
##                                             stringsAsFactors = FALSE))            
##         Journal(id = ans$id,
##                   instrument = ans$instrument,
##                   account = ans$account,
##                   timestamp = ans$timestamp,
##                   amount = ans$amount,
##                   price = ans$price)
##     } else
##         tls        
## }
c.Journal <- function(...) {
    tls <- list(...)
    if (!all(unlist(lapply(tls, "class")) == "Journal"))
        stop("all ... must be Journals")
    
    ns <- unique(unlist(lapply(tls, names)))
    ans <- vector("list", length = length(ns))
    names(ans) <- ns
    for (n in seq_along(ns)) {
        ans[[ns[n]]] <- unlist(lapply(tls, `[[`, ns[n]))
    }
    class(ans) <- "Journal"
    ans
}
