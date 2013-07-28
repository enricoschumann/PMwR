## remove space at beginning or end of string
rmspace <- function(s) {
    s <- gsub("^\\s*", "", s)
    gsub("\\s*$", "", s)
}

## takes a string like "12.000,23" and returns 12000.23
char2num <- function(s, dec = ",", big.mark = ".") {
    s <- gsub(big.mark, "", s, fixed = TRUE)
    as.numeric(sub(dec, ".", s, fixed = TRUE))
}

.TeXunit.table <- c("cm" = 1864680,
                    "in" = 4736287)

## convert from one TeXunit to another
TeXunits <- function(from, to, from.unit = NULL) {
    if (!is.null(from.unit))
        frU <- from.unit
    else
        frU <- gsub("([-+0-9,. ])+([a-z]+) *", "\\2", from)
    fr <- gsub("([-+0-9,. ]+)([a-z]+) *", "\\1", from)
    fr <- as.numeric(gsub(",", ".", fr))
    fr * .TeXunit.table[frU] / .TeXunit.table[to]
    
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
