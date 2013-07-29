position <- function(amount, timestamp, instrument, when, from, to,
                     drop.zero = FALSE) {
    if (missing(instrument))
        instrument <- NA
    if (inherits(amount, "journal")) {
        J <- amount
        instrument <- J$instrument
        timestamp  <- J$timestamp
        amount     <- J$amount
    } else if (inherits(timestamp, "journal")) {
        J <- timestamp
        instrument <- timestamp$instrument
        amount <- timestamp$amount
        timestamp <- timestamp$timestamp
    } else if (inherits(instrument, "journal")) {
        J <- instrument
        amount <- instrument$amount
        timestamp <- instrument$timestamp
        instrument <- instrument$instrument
    }

    if (missing(when)) {
        when <- max(timestamp)
    } else if (is.character(when)) {
        if (when[1L] == "last" || when[1L] == "newest" || when[1L] == "latest")
            when <- max(timestamp)
        else if (when[1L] == "all")
            when <- timestamp
        else if (when[1L] == "first" || when[1L] == "oldest")
            when <- min(timestamp)
    }

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
    if (is.null(instrument)) 
        instrument <- rep.int("", length(timestamp))    
    allna <- FALSE
    if (all(is.na(instrument))) {
        instrument[] <- ""
        allna <- TRUE
    }
    if (!all(is.na(instrument)))
        instrument[is.na(instrument)] <- "not specified"

    nw <- length(when)
    nm <- sort(unique(instrument))
    pos <- array(0, dim = c(nw, length(nm)))
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
    if (!is.logical(drop.zero)) {
        drop <- apply(pos, 2, function(x) all(abs(x) < drop.zero))
        pos <- pos[ , !drop, drop = FALSE]
        nm <- nm[!drop]
    } else if (drop.zero) {
        drop <- apply(pos, 2, function(x) all(x == 0))
        pos <- pos[ , !drop, drop = FALSE]
        nm <- nm[!drop]
    }
    if (allna)
        nm[] <- NA
    ans <- list(position = pos, timestamp = when, instrument = nm)
    class(ans) <- "position"
    ans
}

print.position <- function(x, ..., sep = NA){

    if (!is.na(sep))
        stop("'sep' is not yet implemented")
    if (dim(x$position)[1L] > 1L)
        print(x$position, big.mark = ",")
    else
        print(t(x$position), big.mark = ",")
    invisible(x)
}
