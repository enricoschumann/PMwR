position <- function(amount, timestamp, instrument, when, from, to, ...) {
    if (missing(instrument))
        instrument <- NA
    if (missing(when))
        when <- max(timestamp)
    if (inherits(amount, "Tradelist")) {
        instrument <- amount$instrument
        timestamp <- amount$timestamp
        amount <- amount$amount
    } else if (inherits(timestamp, "Tradelist")) {
        instrument <- timestamp$instrument
        amount <- timestamp$amount
        timestamp <- timestamp$timestamp
    } else if (inherits(instrument, "Tradelist")) {
        amount <- instrument$amount
        timestamp <- instrument$timestamp
        instrument <- instrument$instrument
    }
    if (is.unsorted(timestamp)) {
        io  <- order(timestamp)
        timestamp <- timestamp[io]
        amount  <- amount[io]
        instrument <- instrument[io]
    }
    instrument[is.na(instrument)] <- "not specified"
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
            pos[j, nm[i]] <- if (length(beforewhen))
                cumsum(iv)[max(beforewhen)] else 0
        }
    }
    drop(pos)
}
