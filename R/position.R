position <- function(amount, ...)
    UseMethod("position")

position.default <- function(amount, timestamp, instrument,
                             when, drop.zero = FALSE,
                             account = NULL, ...) {
    
    no.instruments <- FALSE ## are all instruments missing/NA?
    if (missing(instrument) ||
        !length(instrument) ||
        all(is.na(instrument))) {
        instrument <- rep.int("", length(amount))
        no.instruments <- TRUE
    }

    no.timestamp <- FALSE
    if (missing(timestamp) ||
        !length(timestamp) ||
        all(is.na(timestamp))) {
        timestamp <- rep(1, length(amount))
        no.timestamp <- TRUE
    }

    len <- max(length(amount),
               length(timestamp),
               length(instrument),
               length(account))
    amount <- rep(amount, len/length(amount))
    timestamp <- rep(timestamp, len/length(timestamp))
    instrument <- rep(instrument, len/length(instrument))
    account <- rep(account, len/length(account))

    if (missing(when)) {
        ## TODO: if 'when' is missing, we can simply sum the amounts        
        when <- max(timestamp, na.rm = TRUE)
    } else {
        if (no.timestamp)
            warning(sQuote("when"),
                    " specified, but no valid timestamp supplied")
        if (is.character(when)) {            
            if (when[[1L]] == "last" ||
                when[[1L]] == "newest" ||
                when[[1L]] == "latest")
                when <- max(timestamp)
            else if (when[[1L]] == "all")
                when <- unique(timestamp)
            else if (when[[1L]] == "endofmonth" ||
                     when[[1L]] == "lastofmonth") {
                ## when <- last(timestamp,
                ##              format(as.Date(timestamp), "%Y-%m"))
                timestamp <- as.Date(timestamp)
                when <- endOfMonth(seq(firstOfMonth(min(timestamp)),
                                       firstOfMonth(max(timestamp)),
                                       by = "1 month"))
            } else if (when[[1L]] == "endofday") {
                when <- last(timestamp,
                             format(as.Date(timestamp), "%Y-%m-%d"))
            } else if (when[[1L]] == "first" ||
                     when[1L] == "oldest")
                when <- min(timestamp)
        }
    }

    if (!anyNA(timestamp) && is.unsorted(timestamp)) {
        io <- order(timestamp)
        timestamp <- timestamp[io]
        amount  <- amount[io]
        instrument <- instrument[io]
        if (!is.null(account))
            account <- account[io]
    }

    if (anyNA(timestamp) && is.unsorted(timestamp, na.rm = TRUE))
        stop("cannot compute position: journal is not sorted ",
             "and timestamp has NA values")

    if (anyNA(timestamp) && !is.unsorted(timestamp, na.rm = TRUE))
        warning("timestamp has NA values")

    ## if (all(ina <- is.na(instrument))) {
    ##     instrument[] <- ""
    ##     no.instruments <- TRUE
    ## } else
    ##     instrument[ina] <- "NA"

    if (!is.null(account) && !identical(account, FALSE))                      
        instrument <- paste(account, "%SEP%", instrument, sep = "")
        
    nw <- length(when)
    nm <- sort(unique(instrument))
    pos <- array(0, dim = c(nw, length(nm)))
    colnames(pos) <- nm
    rownames(pos) <- if (no.timestamp) rep("", length(when)) else as.character(when)
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
    if (no.instruments)
        nm[] <- NA
    attr(pos, "timestamp") <- if (no.timestamp) NA else when
    attr(pos, "instrument") <- gsub(".*%SEP%(.*?)", "\\1", nm)
    if (!is.null(account))         
        attr(pos, "account") <- gsub("(.*)%SEP%.*", "\\1", nm)    
    class(pos) <- "position"
    pos
}

position.journal <- function(amount, when,
                             drop.zero = FALSE,
                             use.account = FALSE, ...) {

    instrument <- amount$instrument
    timestamp  <- amount$timestamp
    amount     <- amount$amount
    account <- if (use.account)
                   amount$account

    position.default(amount, timestamp, instrument, when,
                     drop.zero = drop.zero,
                     account = account, ...)
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
    ## if (all.equal(dimnames(x), list(NULL, NULL))) {
    ##     if (any(dim(x) == 1L))
    ##         print(c(unclass(x)))
    ##     else
    ##         dimnames(x) <- list(rep("", dim(x)[[1L]]), rep("", dim(x)[[2L]]))
    ## } else
    if (dim(x)[1L] > 1L) {
        print(unclass(x))
    } else {
        print(t(unclass(x)))
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

Ops.position <- function(e1, e2) {
    if (nargs() == 1) {
        switch(.Generic, `+` = {
        }, `-` = {
            e1[] <- -unclass(e1)
        },
        ## stop(gettextf("unary '%s' not defined for ",
        ##               sQuote("position"), " objects", 
        ##               .Generic), domain = NA, call. = FALSE)
        NextMethod(.Generic)
        )
        return(e1)
    }

    if (inherits(e1, "position") && inherits(e2, "position")) {
        allI <- sort(unique(c(instrument(e1), instrument(e2))))
        ans <- numeric(length(allI))
        ans[match(instrument(e1), allI)] <- as.numeric(e1)
        ii <- match(instrument(e2), allI)
        if (.Generic == "+") {
            ans[ii] <- ans[ii] + as.numeric(e2)
        } else if (.Generic == "-") {
            ans[ii] <- ans[ii] - as.numeric(e2)
        } else if (.Generic == "/") {
            ans[ii] <- ans[ii] / as.numeric(e2)
        }
        position.default(ans, instrument = allI,
                                timestamp = rep(attr(e1, "timestamp"),
                                                length(allI)))
    } else
        NextMethod(.Generic)
}

acc.split <- function(account, sep, perl = FALSE, tree = FALSE) {

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

    
    ## LEVEL
    level <- as.numeric(
        unlist(lapply(gregexpr(sep, ans),
                      function(x) length(x) == 1 && x == -1)))
    level[level == 0L] <- lengths(gregexpr(sep, ans))[level == 0L] + 1


    ## TREE
    if (tree) {
        leaf <- function(x, sep = "::") {
            ## return last subaccount 
            ## account::subaccount::...::deepest_subaccount
            gsub(paste0(".*", sep, "([^", substr(sep,1,1), "]+)$"),
                 "\\1", x, perl = TRUE)
        }
        
        sp <- spaces(4*(level - 1))
        tree1 <- paste0(sp, leaf(ans)) 

        tree2 <- paste0(.tree(level), leaf(ans))
        tree3 <- paste0(.tree(level, TRUE), leaf(ans))
        tree3 <- enc2utf8(tree3)
        
        data.frame(account = ans,
                   level,
                   tree_indent  = tree1,
                   tree_ascii   = tree2,
                   tree_unicode = tree3,
                   stringsAsFactors = FALSE)
    } else {
        data.frame(account = ans, level, stringsAsFactors = FALSE)
    }    
} 

as.zoo.position <- function(x, ...) {
    zoo(x, attr(x, "timestamp"))
}

.tree <- function(lv, unicode = FALSE) {
    child <- "|--"
    final_child <- "`"
    cont <- "|"
    indent <- spaces((lv-1)*4)
    mlv <- max(lv)
    n <- length(lv)
    for (i in 1L:(mlv-1L)) {
        group.start <- c(which(lv == i), n+1)
        group.end <- group.start
        for (g in 1:length(group.start)) {
            if (any(next_l <- 1:n < group.start[g+1] &
                              1:n >= group.start[g] &
                              lv == i+1))
                group.end[g] <- max(which(next_l))
        }
        group.start <- group.start[-length(group.start)]
        group.end <- group.end[-length(group.end)]
        
        for (g in 1:length(group.start)) {
            if (group.start[g] < group.end[g])
                substring(indent[setdiff((group.start[g]+1):group.end[g], which(lv == 1+1))],
                (i-1)*4+1,(i-1)*4+1) <- cont
        }
        substring(indent[lv==1+i],(i-1)*4+1,(i-1)*4+3) <- child
        substring(indent[group.end[group.end != group.start]],
               (i-1)*4+1, (i-1)*4+1) <- final_child
    }
    if (unicode) {
        indent <- gsub("|--", "\u251c\u2500\u2500", indent, fixed = TRUE)
        indent <- gsub("`--", "\u2514\u2500\u2500", indent, fixed = TRUE)
        indent <- gsub("|", "\u2502", indent, fixed = TRUE)
    }
    indent
}

