position <- function(amount, ...)
    UseMethod("position")

position.journal <- function(amount, when,
                             drop.zero = FALSE, sort.instruments = TRUE, ...) {

    instrument <- amount$instrument
    timestamp  <- amount$timestamp
    amount     <- amount$amount

    position.default(amount, timestamp, instrument, when, 
                     drop.zero = drop.zero,
                     sort.instruments = sort.instruments, ...)
}
                             
position.default <- function(amount, timestamp, instrument, when, 
                             drop.zero = FALSE, sort.instruments = TRUE, ...) {
    if (missing(instrument))
        instrument <- NA
        
    if (missing(timestamp) || !length(timestamp)){
        if (!missing(when))
            warning(sQuote("when"), " specified, but no valid timestamp supplied")
        timestamp <- rep(1, length(amount))        
    }

    if (missing(when) && all(is.na(timestamp))) {
        when <- ""
        timestamp <- rep("", length(amount))        
    } else if (missing(when)) {
        ## TODO: if 'when' is missing, we can simply sum the amounts
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

    if (any(is.na(timestamp)) && is.unsorted(timestamp,na.rm = TRUE)) {
        stop("cannot compute position: journal is not sorted and timestamp has NA values ")
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
    ## ans <- list(position = pos, timestamp = when, instrument = nm)
    ## ans <- pos
    attr(pos, "timestamp") <- when
    attr(pos, "instrument") <- nm    
    class(pos) <- "position"
    pos
}

position.btest <- function(amount, when, ...) {


}

print.position <- function(x, ..., sep = NA) {
    original.x <- x
    if (!is.na(sep))
        .NotYetUsed("sep")
    instrument <- attr(x, "instrument")
    timestamp <- attr(x, "timestamp")
    if (all(is.na(timestamp)) || (is.character(timestamp) && all(timestamp == "")))
        rownames(x) <- NULL
    if (all(is.na(instrument)))
        colnames(x) <- instrument
    attr(x, "instrument") <- NULL
    attr(x, "timestamp") <- NULL
    if (dim(x)[1L] > 1L) {
        print(unclass(x), big.mark = ",")
    } else {
        print(t(unclass(x)), big.mark = ",")
    }
    invisible(original.x)
}

## `[.position`  <- function(x, i, j, ...) {
##     if (missing(i))
##         i <- TRUE
##     if (missing(j))
##         j <- TRUE
##     ans <- x$position[i,j, drop = FALSE]
##     ans <- list(position = ans,
##                 timestamp = x$timestamp[i],
##                 instrument = x$instrument[j])
##     class(ans) <- "position"
##     ans
## }
