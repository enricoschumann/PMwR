Tradelist <- function(timestamp, amount, price, id, instrument, account, ...) {
    
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
    ans <- list(id = id,
                instrument = instrument,
                account    = account,
                timestamp  = timestamp,
                amount     = amount,
                price      = price)
    dots <- list(...)
    nd <- names(dots)
    if (length(dots)) {
        if (any(nd == "") || is.null(nd))
            stop("arguments passed via ... must be named")
        else
            ans <- c(ans, dots)
    }    
    class(ans) <- "Tradelist"
    ans    
}
print.Tradelist <- function(x, ...) {
    oo <- getOption("scipen")
    options(scipen = 1e8)
    on.exit(options(scipen = oo))

    dspT <- 10 ## display trades, instruments
    dspI <- 8
    
    if (all(!is.na(x$account)))
        rn <- x$account        
    if (all(!is.na(x$id)))
        rn <- paste(rn, x$id, sep = " | ")

    print(head(data.frame(timestamp = x$timestamp,
                          instrument = x$instrument,
                          amount = x$amount,
                          price = x$price,
                          row.names = seq_len(length(x$amount)),
                          stringsAsFactors = FALSE), dspT))
    if ((n <- max(length(x$amount),length(x$price))) > dspT)
        cat("...\n")
    insts <- sort(unique(x$instrument))
    if (length(insts) > dspI) {
        insts <- insts[seq_len(dspI)]
        insts[dspI] <- "..."
    }
    if (length(insts))
        subs <- paste0(" in ", paste(insts, collapse = ", "))
    else
        subs <- ""
    cat(paste0("(", n, " trades", subs, ")\n"))
    invisible(x)
}
sort.Tradelist <- function(x, ...) {
    cat("not implemented\n")
    invisible(x)
}
filterTradelist <- function(x, timestamp, amount, price,
                            id, instrument, account, ...) {    
    cat("not implemented\n")
    invisible(x)
}
c.Tradelist <- function(...) {
    tls <- list(...)
    if (length(tls) > 1L) {
        ans <- as.data.frame(unclass(tls[[1L]]), stringsAsFactors = FALSE)
        for (i in 2:length(tls))
            ans <- rbind(ans, as.data.frame(unclass(tls[[i]]),
                                            stringsAsFactors = FALSE))            
        Tradelist(id = ans$id,
                  instrument = ans$instrument,
                  account = ans$account,
                  timestamp = ans$timestamp,
                  amount = ans$amount,
                  price = ans$price)
    } else
        tls        
}
c.Tradelist <- function(...) {
    tls <- list(...)
    if (!all(unlist(lapply(tls, "class")) == "class"))
        stop("all ... must be Tradelists")
    
    ns <- unique(unlist(lapply(tls, names)))
    for (n in seq_along(ns)) {
        lapply(tls, `[[`, ns[n])
    }
}
