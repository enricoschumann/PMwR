## -*- truncate-lines: t; -*-
## Copyright (C) 2008-18  Enrico Schumann

is_valid_ISIN <- function(isin) {    

    if (as.character(sys.call(sys.parent()))[1L] == "isValidISIN")
        .Deprecated("is_valid_ISIN")

    validNC <- 12L
    result <- logical(length(isin))
    isin <- isin[hasValidNC <- which(nchar(isin) == validNC)]
    if (!length(isin))
        return(result)
    isin <- toupper(isin)
    pasteAndSplit <- function(s)
        as.numeric(unlist(strsplit(paste(s, collapse = ""), "")))
    multSecby2 <- function(x) {
        lx <- length(x)
        i <- seq(lx %% 2, lx, by = 2)
        x[i] <- x[i]*2
        x
    }
    cd <- as.integer(substr(isin, validNC, validNC))  ## control digit
    v0 <- substr(isin, 1L, validNC - 1L) ## v0 is vector
    v0 <- strsplit(v0, "")  ## get list of char vecs; v0 is list
    v0 <- lapply(v0, function(x) match(x, c(as.character(0:9), LETTERS)) - 1L)
    v0 <- lapply(v0, pasteAndSplit)
    v0 <- lapply(v0, multSecby2)
    v0 <- lapply(v0, pasteAndSplit)

    result[hasValidNC] <- (10L - sapply(v0, sum) %% 10L) %% 10L == cd
    result
}

.ISIN <- "[^A-Za-z]*([A-Za-z][A-Za-z][A-Za-z0-9]{9,9}[0-9])[^0-9]*"
