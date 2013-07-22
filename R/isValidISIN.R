isValidISIN <- function(isin) {    
    validNC <- 12L
    result <- logical(length(isin))
    isin <- isin[hasValidNC <- which(nchar(isin) == validNC)]
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

    result[hasValidNC] <- (10L - sapply(v0, sum) %% 10L) == cd
    result
}
