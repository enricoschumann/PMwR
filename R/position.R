position <- function(amount, timestamp, instrument, when, from, to, nonzero.only = FALSE,
                     aggr.accounts = FALSE, account.sep = "::") {
    if (missing(instrument))
        instrument <- NA
    if (inherits(amount, "Journal")) {
        J <- amount
        instrument <- J$instrument
        timestamp  <- J$timestamp
        amount     <- J$amount
    } else if (inherits(timestamp, "Journal")) {
        J <- timestamp
        instrument <- timestamp$instrument
        amount <- timestamp$amount
        timestamp <- timestamp$timestamp
    } else if (inherits(instrument, "Journal")) {
        J <- instrument
        amount <- instrument$amount
        timestamp <- instrument$timestamp
        instrument <- instrument$instrument
    }

    if (missing(when))
        when <- max(timestamp)

    if (!missing(from)) {
        if (missing(to))
            to <- max(timestamp)
        keep <- timestamp < from & timestamp > to
        amount <- amount[keep]
        timestamp <- timestamp[keep]
        instrument <- instrument[keep]
    }
        
    if (!any(is.na(timestamp)) && is.unsorted(timestamp)) {
        io  <- order(timestamp)
        timestamp <- timestamp[io]
        amount  <- amount[io]
        instrument <- instrument[io]
    }
    if (!all(is.na(instrument)))
        instrument[is.na(instrument)] <- "not specified"
    if (all(is.na(instrument)))
        instrument[] <- ""

    
    if (!aggr.accounts && exists("J") && !all(is.na(J$account)) && length(unique(J$account)) > 1L) {
        instrument <- paste0(J$account, account.sep, instrument)
        by.account <- TRUE
    } else 
        by.account <- FALSE

    nw <- length(when)
    pos <- array(0, dim = c(nw,
                    length(nm <- sort(unique(instrument)))))
    colnames(pos) <- nm        
    rownames(pos) <- as.character(when)
    for (j in seq_len(nw)) {
        for (i in seq_along(nm)) {
            ri  <-  nm[i] == instrument
            idt <- timestamp[if (length(timestamp) == 1L) 1 else ri]
            iv  <- amount[if (length(amount) == 1L) 1 else ri]
            beforewhen <- which(when[j] >= idt)
            pos[j, i] <- if (length(beforewhen))
                cumsum(iv)[max(beforewhen)] else 0
        }
    }
    pos
}
