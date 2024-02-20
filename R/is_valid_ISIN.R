## -*- truncate-lines: t; -*-
## Copyright (C) 2008-24  Enrico Schumann

.ISIN <- "[^A-Za-z]*([A-Za-z][A-Za-z][A-Za-z0-9]{9,9}[0-9])[^0-9]*"

is_valid_ISIN <- function(isin, NA.FALSE = FALSE) {

    if (as.character(sys.call(sys.parent()))[1L] == "isValidISIN")
        .Deprecated("is_valid_ISIN")

    result <- logical(length(isin))
    names(result) <- isin

    ## any NA?
    i.NA <- is.na(isin)
    good <- !i.NA
    ## ==> now, everything that is not NA is considered good

    ## number of chars == 12?
    validNC <- 12L
    good[good] <- good[good] &
                  nchar(isin[good]) == validNC

    ## check digits %in% 0:9?
    cd <- substr(isin[good], validNC, validNC)
    good[good] <- good[good] &
                  cd %in% c("0", "1", "2", "3", "4",
                            "5", "6", "7", "8", "9")

    isin <- isin[good]
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
    v0 <- substr(isin, 1L, validNC - 1L)              ## v0 is vector
    v0 <- strsplit(v0, "")  ## get list of char vecs; v0 is list
    v0 <- lapply(v0, function(x) match(x, c(as.character(0:9), LETTERS)) - 1L)
    v0 <- lapply(v0, pasteAndSplit)
    v0 <- lapply(v0, multSecby2)
    v0 <- lapply(v0, pasteAndSplit)

    result[good] <- (10L - sapply(v0, sum) %% 10L) %% 10L == cd
    result[i.NA] <- if (NA.FALSE) FALSE else NA
    result
}

is_valid_SEDOL <- function(SEDOL, NA.FALSE = FALSE) {

    ## TODO strip hyphens, as in B-1H5-4P6 ?

    i.NA <- is.na(SEDOL)
    SEDOL[i.NA] <- "A"

    reason <- rep("", length(SEDOL))
    is_valid <- !logical(length(SEDOL))
    names(reason) <- names(is_valid) <- SEDOL

    SEDOL <- toupper(SEDOL)


    ## Test length == 7
    i <- nchar(SEDOL) != 7L
    is_valid[i] <- FALSE
    reason[i] <- "SEDOL must have 7 digits"

    ## TODO separate test for invalid characters?
    if (any(is_valid)) {
        S <- strsplit(SEDOL[is_valid], "")
        valid.chars <- c("0", "1", "2", "3", "4", "5", "6",
                         "7", "8", "9", "B", "C", "D", "F",
                         "G", "H", "J", "K", "L", "M", "N",
                         "P", "Q", "R", "S", "T", "V", "W",
                         "X", "Y", "Z")
        i <- sapply(S, function(x) !all(x %in% valid.chars))
        reason[is_valid][i] <- "invalid characters"
        is_valid[is_valid][i] <- FALSE
    }

    ## Test check digit
    if (any(is_valid)) {
        values <- c(`0` = 0, `1` = 1, `2` = 2, `3` = 3, `4` = 4,
                    `5` = 5, `6` = 6, `7` = 7, `8` = 8, `9` = 9,
                    B = 11, C = 12, D = 13, F = 15, G = 16, H = 17,
                    J = 19, K = 20, L = 21, M = 22, N = 23, P = 25,
                    Q = 26, R = 27, S = 28, T = 29, V = 31, W = 32,
                    X = 33, Y = 34, Z = 35)
        weights <- c(1, 3, 1, 7, 3, 9, 1)

        S <- strsplit(SEDOL[is_valid], "")
        check.sums <- sapply(S, function(x) sum(values[x] * weights))
        i <- check.sums %% 10 !=0
        reason[is_valid][i] <- "check digit is incorrect"
        is_valid[is_valid][i] <- FALSE
    }

    ## Reinstate NAs
    is_valid[i.NA] <- if (NA.FALSE) FALSE else NA
    reason[i.NA] <- if (NA.FALSE) paste(sQuote("NA.FALSE"), "is TRUE") else ""
    names(reason)[i.NA] <- names(is_valid)[i.NA] <- NA

    attr(is_valid, "note") <- reason
    is_valid
}
