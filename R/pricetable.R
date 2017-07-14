## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

pricetable <- function(instrument, when, price,
                       price.instrument = NULL, price.timestamp = NULL,
                       missing = "error") {

    if (!is.matrix(price))
        price <- as.matrix(price)
    
    if (is.null(price.instrument) && !is.null(colnames(price)))
        price.instrument <- colnames(price)
    
    if (missing(instrument)) {
        ## no instrument
        ## ==> compute prices for each column in price
        if (!is.null(price.instrument))
            instrument <- price.instrument
        else if (is.null(price.instrument))
            price.instrument <-
                instrument  <- paste0("i", seq_len(ncol(price)))
    }
    
    if (is.null(price.timestamp) && !is.null(rownames(price)))
        price.timestamp <- rownames(price)
    else if (is.null(price.timestamp))
        price.timestamp <- seq_len(nrow(price))
    

    if (length(miss <- setdiff(instrument, price.instrument))) {
        if (is.na(missing) || missing == "NA") {
            tmp <- array(NA, dim = c(nrow(price), length(miss)))
            colnames(tmp) <- miss
            price <- cbind(price, tmp)            
        } else if (tolower(missing) == "error") {
            stop("no data for instrument(s) ", paste(miss, collapse = ", "))
        } else {
            tmp <- array(missing, dim = c(nrow(price), length(miss)))
            colnames(tmp) <- miss
            price <- cbind(price, tmp)
        }
    }
        
    i <- matchOrPrevious(when, price.timestamp)
    j <- match(instrument, price.instrument)
    ans <- price[i, j, drop = FALSE]
    colnames(ans) <- instrument
    rownames(ans) <- as.character(when)
    class(ans) <- "pricetable"
    attr(ans, "instrument") <- instrument
    attr(ans, "timestamp")  <- when
    ans
}




pricetable <- function(price, ...) {
    UseMethod("pricetable")
}

pricetable.matrix <- function(price, instrument, timestamp, missing = "NA",
                              ...) {

    if (missing(instrument))
        if (is.null(instrument <- colnames(price)))
            instrument <- seq_len(ncol(price))

    if (missing(timestamp))
        if (is.null(timestamp <- rownames(price)))
            timestamp <- seq_len(nrow(price))

    if (anyDuplicated(instrument))
        warning("duplicate instruments")

    if (anyDuplicated(timestamp))
        warning("duplicate timestamps")

    ans <- price
    colnames(ans) <- attr(ans, "instrument") <- instrument
    rownames(ans) <- attr(ans, "timestamp") <- timestamp

    class(ans) <- "pricetable"
    ans
    
}


`[.pricetable`  <- function(p, i, j, start, end, ..., drop = FALSE) {


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

    p.raw <- unclass(p)
    
    if (is.character(j)) {
        j <- match(j, attr(p, "instrument"), nomatch = 0L)
    }

    ans <- array(NA, dim = c(length(i), length(j)))

    
    ans <- unclass(p)[i,j, drop = drop]
    class(ans) <- "pricetable"
    ans
}

`[<-.pricetable`  <- function(x, i, j, ..., value) {
    stop("extraction only: convert to matrix to replace values")
}

## x <- array(1:6, dim = c(3,2))
## colnames(x) <- letters[1:2]
## rownames(x) <- 1:3

## p <- pricetable(x)
## p[ , c("a", "a", "c")]
