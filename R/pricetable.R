## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

## pricetable <- function(instrument, when, price,
##                        price.instrument = NULL, price.timestamp = NULL,
##                        missing = "error") {

##     if (!is.matrix(price))
##         price <- as.matrix(price)
    
##     if (is.null(price.instrument) && !is.null(colnames(price)))
##         price.instrument <- colnames(price)
    
##     if (missing(instrument)) {
##         ## no instrument
##         ## ==> compute prices for each column in price
##         if (!is.null(price.instrument))
##             instrument <- price.instrument
##         else if (is.null(price.instrument))
##             price.instrument <-
##                 instrument  <- paste0("i", seq_len(ncol(price)))
##     }
    
##     if (is.null(price.timestamp) && !is.null(rownames(price)))
##         price.timestamp <- rownames(price)
##     else if (is.null(price.timestamp))
##         price.timestamp <- seq_len(nrow(price))
    

##     if (length(miss <- setdiff(instrument, price.instrument))) {
##         if (is.na(missing) || missing == "NA") {
##             tmp <- array(NA, dim = c(nrow(price), length(miss)))
##             colnames(tmp) <- miss
##             price <- cbind(price, tmp)            
##         } else if (tolower(missing) == "error") {
##             stop("no data for instrument(s) ", paste(miss, collapse = ", "))
##         } else {
##             tmp <- array(missing, dim = c(nrow(price), length(miss)))
##             colnames(tmp) <- miss
##             price <- cbind(price, tmp)
##         }
##     }
        
##     i <- matchOrPrevious(when, price.timestamp)
##     j <- match(instrument, price.instrument)
##     ans <- price[i, j, drop = FALSE]
##     colnames(ans) <- instrument
##     rownames(ans) <- as.character(when)
##     class(ans) <- "pricetable"
##     attr(ans, "instrument") <- instrument
##     attr(ans, "timestamp")  <- when
##     ans
## }


pricetable <- function(price, ...) {
    UseMethod("pricetable")
}

pricetable.default <- function(price, instrument, timestamp, ...) {

    price <- as.matrix(price)

    if (missing(instrument))
        if (is.null(instrument <- colnames(price)))
            instrument <- paste0("P", seq_len(ncol(price)))

    if (missing(timestamp))
        if (is.null(timestamp <- rownames(price)))
            timestamp <- seq_len(nrow(price))

    if (anyDuplicated(instrument))
        warning("duplicate instruments")

    if (anyDuplicated(timestamp))
        warning("duplicate timestamps")

    ans <- price
    attr(ans, "instrument") <- instrument
    attr(ans, "timestamp") <- timestamp

    colnames(ans) <- as.character(instrument)
    rownames(ans) <- as.character(timestamp)
    
    class(ans) <- "pricetable"
    ans
    
}

pricetable.zoo <- function(price, instrument, ...) {

    timestamp <- index(price)
    pricetable.default(coredata(price),
                       instrument = instrument,
                       timestamp = timestamp, ...)
}


`[.pricetable`  <- function(p, i, j, start, end, missing = NA,..., drop = FALSE) {


    ## pt[when, instrument]
    ##
    ## i  .. character, logical, numeric, datetime
    ## j  .. character, logical, numeric
    ## answer is guaranteed to have dim(length(i), length(j))
    
    
    ## if (is.character(i)) {
    ##     if (is.null(match.against))
    ##         match.against  <- names(x)[unlist(lapply(x, mode)) == "character"]
    ##     ii <- logical(length(x))
    ##     if (length(i) > 1L)
    ##         i <- paste(i, collapse = "|")
    ##     for (m in match.against) {
    ##         if (is.null(x[[m]]))
    ##             next
    ##         ii <- ii | grepl(i, x[[m]], ignore.case = ignore.case, ...)
    ##     }        
    ##     if (reverse)
    ##         ii <- !ii
    ## } else
    ##     ii <- i

    timestamp <- attr(p, "timestamp")
    instrument <- attr(p, "instrument")

    if (missing(i)) {
        i <- timestamp
        i.orig <- i
    } else {
        i.orig <- i
        if (is.character(i)) {
            if (inherits(timestamp, "Date")) {
                i <- as.Date(i)
                i.orig <- as.character(i)
            } else if (inherits(timestamp, "POSIXct"))
                i <- as.POSIXct(i)
        }
        i <- match(i, timestamp, nomatch = 0L)        
    } 


    if (missing(j)) {
        if (is.null(j <- colnames(p)))
            j <- TRUE
        j.orig <- j
    } else {
        j.orig <- j
        j <- match(j, instrument, nomatch = 0L)
    }
    

    ans <- array(NA, dim = c(length(i), length(j)))
        
    ans[i>0, j>0] <- unclass(p)[i[ i> 0], j[ j> 0], drop = FALSE]

    attr(ans, "timestamp") <- i.orig
    attr(ans, "instrument") <- j.orig

    rownames(ans) <- as.character(i.orig)
    colnames(ans) <- as.character(j.orig)

    if (!is.na(missing)) {
        ans[is.na(ans)] <- missing
    }
    class(ans) <- "pricetable"

    ans
}

`[<-.pricetable`  <- function(x, i, j, ..., value) {
    stop("extraction only: convert to matrix to replace values")
}

print.pricetable <- function(x, ...) {
    xx <- x
    attr(xx, "timestamp") <- NULL
    attr(xx, "instrument") <- NULL
    print.default(unclass(xx))
}

