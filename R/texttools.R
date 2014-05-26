## remove space at beginning or end of string
rmspace <- function(s, leading = TRUE, trailing = TRUE) {
    if (leading)
        s <- gsub("^\\s*", "", s)
    if (trailing)
        s <- gsub("\\s*$", "", s)
    s
}

## takes a string like "12.000,23" and returns 12000.23
char2num <- function(s, dec = ",", big.mark = ".") {
    s <- gsub(big.mark, "", s, fixed = TRUE)
    as.numeric(sub(dec, ".", s, fixed = TRUE))
}

.TeXunit.table <- c("cm" = 1864680,
                    "in" = 4736287)

## remove repeated pattern
rmrp <- function(s, pattern, ...) {
    i <- grep(pattern, s, ...)
    if (any(ii <- diff(i) == 1L))
        s <- s[-i[which(c(FALSE, ii))]]    
    s
}


## remove blank lines at beginning/end
rmbl <- function(s, pattern = "^$| +", ..., leading=TRUE , trailing=TRUE) {
s <- c("",""," ", "sahs", "jwhd", "", "", "", "", "", "")

    m <- grep(pattern, s)
    rm <- NULL
    if (leading && match(1, m, nomatch = 0L))
        rm <- seq_len(which(diff(m) > 1L)[1L])  
    if (trailing && match(length(s), m, nomatch = 0L))
        stop("to be written")
    s[-rm]    
}

## convert from one TeXunit to another
TeXunits <- function(from, to, from.unit = NULL) {
    if (!is.null(from.unit))
        frU <- from.unit
    else
        frU <- gsub("([-+0-9,. ])+([a-z]+) *", "\\2", from)
    fr <- gsub("([-+0-9,. ]+)([a-z]+) *", "\\1", from)
    fr <- as.numeric(gsub(",", ".", fr))
    ans <- fr * unname(.TeXunit.table[frU]) / .TeXunit.table[to]
    if (length(ans) > 1L && length(to) == 1L)
        names(ans) <- rep(to, length(from))
    ans
}


## expand string to given width
expstr <- function(s, after, width, fill = " ", at) {
    ns <- nchar(s)
    space <- character(length(s))
    for (i in seq_along(space))
        space[i] <- paste(rep(" ", width[1L] - ns[i]), collapse = "")
    rx <- regexpr(after, s)
    if (!missing(after))
        at <- as.numeric(rx+ attr(rx, "match.length"))
    paste(substr(s, 1L, at - 1L), space,
          substr(s, at, ns), sep = "")    
}

## pretty print a csv file
