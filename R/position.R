position <- function(amount, ...)
    UseMethod("position")

position.journal <- function(amount, when,
                             drop.zero = FALSE,  ...) {

    instrument <- amount$instrument
    timestamp  <- amount$timestamp
    account    <- amount$account
    amount     <- amount$amount

    position.default(amount, timestamp, instrument, when,
                     drop.zero = drop.zero,
                     account = account, ...)
}

position.default <- function(amount, timestamp, instrument,
                             when, drop.zero = FALSE,
                             account = NULL, ...) {

    allna <- FALSE ## are all instruments missing/NA?
    if (missing(instrument) ||
        is.null(instrument) ||
        all(is.na(instrument)) ||
        !length(instrument)) {
        instrument <- rep.int("", length(amount))
        allna <- TRUE
    }
    if (missing(timestamp) || !length(timestamp)) {
        if (!missing(when))
            warning(sQuote("when"),
                    " specified, but no valid timestamp supplied")
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

    if (!anyNA(timestamp) && is.unsorted(timestamp)) {
        io  <- order(timestamp)
        timestamp <- timestamp[io]
        amount  <- amount[io]
        instrument <- instrument[io]
    }

    if (anyNA(timestamp) && is.unsorted(timestamp, na.rm = TRUE))
        stop("cannot compute position: journal is not sorted ",
             "and timestamp has NA values")

    if (anyNA(timestamp) && !is.unsorted(timestamp, na.rm = TRUE))
        warning("timestamp has NA values")


    if (all(ina <- is.na(instrument))) {
        instrument[] <- ""
        allna <- TRUE
    } else
        instrument[ina] <- "NA"

    instrument <- paste(account, "%SEP%", instrument, sep = "")
        
    nw <- length(when)
    nm <- sort(unique(instrument))
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
        pos <- pos[ , is.na(drop) | !drop, drop = FALSE]
        nm <- nm[is.na(drop) | !drop]
    } else if (drop.zero) {
        drop <- apply(pos, 2, function(x) all(x == 0))
        pos <- pos[ , is.na(drop) | !drop, drop = FALSE]
        nm <- nm[is.na(drop) | !drop]
    }
    if (allna)
        nm[] <- NA
    attr(pos, "timestamp") <- when
    attr(pos, "instrument") <- gsub(".*%SEP%(.*?)", "\\1", nm)
    attr(pos, "account") <- gsub("(.*)%SEP%.*", "\\1", nm)    
    class(pos) <- "position"
    pos
}

position.btest <- function(amount, when, ...) {
    ans <- amount$position
    class(ans) <- "position"
    attr(ans, "timestamp") <- if (!is.null(amount$timestamp))
                                  amount$timestamp else NA
    attr(ans, "instrument") <- if (!is.null(amount$instrument))
                                   amount$instrument else NA
    ans
}

print.position <- function(x, ..., sep = NA) {
    if (dim(x)[[2L]] == 0L) ## empty position
        return(invisible(x))
    original.x <- x
    if (!is.na(sep))
        .NotYetUsed("sep")
    account <- attr(x, "account")
    instrument <- attr(x, "instrument")
    timestamp <- attr(x, "timestamp")
    if (!is.null(account))
        instrument <- paste(account, "  ", instrument, sep = "")
    if (!all(is.na(instrument)))
        colnames(x) <- instrument        

    if (all(is.na(timestamp)) || (is.character(timestamp) && all(timestamp == "")))
        rownames(x) <- NULL
    if (all(is.na(instrument)) || (is.character(instrument) && all(instrument == "")))
        colnames(x) <- NULL

    attr(x, "account") <- NULL
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

as.matrix.position <- function(x, ...) {
    ans <- c(x)
    dim(ans) <- dim(x)

    rownames(ans) <- as.character(attr(x, "timestamp"))
    colnames(ans) <- attr(x, "instrument")
    ans
}

as.data.frame.position <- function(x, ...) {
    ans <- c(x)
    dim(ans) <- dim(x)
    ans <- as.data.frame(ans)
    
    row.names(ans) <- as.character(attr(x, "timestamp"))
    names(ans) <- attr(x, "instrument")
    ans
}

acc.split <- function(account, sep, perl = FALSE) {

    account[is.na(account)] <- ""
    gs <- sort(unique(account))
    
    list.gs <- strsplit(gs, sep, perl = perl)

    ans <- NULL
    for (i in seq_along(list.gs)) {
        if ((lg <- length(list.gs[[i]])) == 1L)
            next
        
        tmp <- NULL
        for (j in seq_len(lg-1))
            tmp <- c(tmp, paste(list.gs[[i]][1:j],
                                collapse = "::"))
        ans <- c(ans, tmp)
    }
    
    ans <- sort(unique(c(ans, account)))

    ## compute level
    level <- as.numeric(
        unlist(lapply(gregexpr(sep, ans),
                      function(x) length(x) == 1 && x == -1)))

    level[level == 0L] <- lengths(gregexpr(sep, ans))[level == 0L] + 1

    data.frame(account=ans, level)
} 

if (FALSE) {

    require("PMwR")
    input <- c("equity::traditional",
               "equity::traditional::USA",
               "equity::traditional::USA",
               "equity::traditional::Europe",
               "equity::traditional::Japan",
               "equity::long-short",
               "equity::long-short",
               "equity::long-short" )
    
    PMwR:::acc.split(input, "\\s*::\\s*", TRUE)
    
    j <- journal(amount = c(1,1,1,1),
                 instrument = c("fgbl", "fgbl", "fesx", "fesx"),
                 account = c("A", "B", "B", "B"),
                 price = c(1,1,1,1),
                 timestamp = 1)
    j2 <- journal(amount = c(1,1,1,1),
                 instrument = c("fgbl", "fgbl", "fesx", "fesx"),
                 account = c("A", "B", "B", "B"),
                 price = c(1,1,1,1),
                 timestamp = 2)
    position(j)
    position(c(j,j2), when=1:2)
    dput(position(j))

}

as.zoo.position <- function(x, ...) {
    zoo(x, attr(x, "timestamp"))
}
