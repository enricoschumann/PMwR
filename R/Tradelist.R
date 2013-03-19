Tradelist <- function(datetime, notional, price, id, instrument, account) {
    
    if (missing(id))
        id <- NA
    if (missing(instrument))
        instrument <- NA
    if (all(instrument == instrument[1L]))
        instrument <- instrument[1L]
    if (missing(account))
        account <- NA
    
    ans <- list(id = id,
                instrument = instrument,
                account = account,
                datetime = datetime,
                notional = notional,
                price = price)
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

    print(head(data.frame(datetime = x$datetime,
                          instrument = x$instrument,
                          notional = x$notional,
                          price = x$price,
                          row.names = seq_len(length(x$notional)),
                          stringsAsFactors = FALSE), dspT))
    if ((n <- length(x$notional)) > dspT)
        cat("...\n")
    insts <- sort(unique(x$instrument))
    if (length(insts) > dspI) {
        insts <- insts[seq_len(dspI)]
        insts[dspI] <- "..."
    }
    cat(paste0("(", n, " trades in ",
               paste(insts, collapse=", "), 
               ")\n"))
    invisible(x)
}
sort.Tradelist <- function(x, ...) {
    cat("not implemented\n")
    invisible(x)
}
filterTradelist <- function(x, datetime, notional, price,
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
                  datetime = ans$datetime,
                  notional = ans$notional,
                  price = ans$price)
    } else
        tls        
}

position <- function(notional, ...) {
    UseMethod("position")
}
position.default <- function(notional, tradetimes, when, from, to, ...) {
    if (is.unsorted(tradetimes)) {
        io  <- order(tradetimes)
        tradetimes <- tradetimes[io]
        notional  <- notional[io]
    }
    beforewhen <- which(when >= tradetimes)
    if (length(beforewhen))
        cumsum(notional)[max(beforewhen)] else 0
}
position.Tradelist <- function(tlist, when, from, to, ...) {
    tlist$instrument[is.na(tlist$instrument)] <- "not specified"
    pos <- numeric(length(nm <- sort(unique(tlist$instrument))))
    names(pos) <- nm
    for (i in seq_along(nm)) {
        ri  <-  nm[i] == tlist$instrument
        idt <- tlist$datetime[if (length(tlist$datetime) == 1L) 1 else ri]
        iv  <- tlist$notional[if (length(tlist$notional) == 1L) 1 else ri]
        pos[nm[i]] <- position(iv, idt, when, from, to)
    }
    pos
}
