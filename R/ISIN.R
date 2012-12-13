isValidISIN <- function(isin) {
    validNC <- 12L
    result <- logical(length(isin))
    isin <- isin[hasValidNC <- which(nchar(isin) == validNC)]
    pasteAndSplit <- function(s)
        as.numeric(unlist(strsplit(paste(s, collapse = ""), "")))
    multSecby2 <- function(x) {
        lx <- length(x)
        i <- seq(lx %% 2, lx, by = 2)
        x[i] <- x[i]*2
        x
    }
    cd <- as.integer(substr(isin, validNC, validNC))
    v0 <- substr(isin, 1L, validNC - 1L)
    v0 <- strsplit(v0, "")
    v0 <- lapply(v0, function(x) match(x, c(as.character(0:9), LETTERS)) - 1L)
    v0 <- sapply(v0, pasteAndSplit)
    v0 <- lapply(v0, multSecby2)
    v0 <- sapply(v0, pasteAndSplit)

    result[hasValidNC] <- (10L - sapply(v0, sum) %% 10L) == cd
    result
}
