position <- function(amount, timestamp, instrument, when, 
                     drop.zero = FALSE, sort.instruments = TRUE) {
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
    if (missing(timestamp) || !length(timestamp)){
        if (!missing(when))
            warning(sQuote("when"), " specified, but no valid timestamp supplied")
        timestamp <- rep(1, length(amount))        
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
        
    if (!any(is.na(timestamp)) && is.unsorted(timestamp)) {
        io  <- order(timestamp)
        timestamp <- timestamp[io]
        amount  <- amount[io]
        instrument <- instrument[io]
    }

    if (is.null(instrument) || !length(instrument)) 
        instrument <- rep.int("", length(amount))    

    allna <- FALSE
    if (all(ina <- is.na(instrument))) {
        instrument[] <- ""
        allna <- TRUE
    } else
        instrument[ina] <- "NA"

    nw <- length(when)
    nm <- unique(instrument)
    if (sort.instruments)
        nm <- sort(nm)
    pos <- array(0, dim = c(nw, length(nm)))
    colnames(pos) <- nm        
    rownames(pos) <- as.character(when)
    for (j in seq_len(nw)) {
        for (i in seq_along(nm)) {
            ri  <-  nm[i] == instrument
            idt <- timestamp[ri]
            iv  <- amount[ri]
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

print.position <- function(x, ..., sep = NA) {

    if (!is.na(sep))
        stop("'sep' is not yet implemented")
    colnames(x$position) <- x$instrument
    if (dim(x$position)[1L] > 1L) {
        print(x$position, big.mark = ",")
    } else {
        ##rownames(x$position) <- ""
        print(t(x$position), big.mark = ",")
    }
    invisible(x)
}
