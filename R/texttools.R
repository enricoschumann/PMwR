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

## convert from one TeXunit to another
TeXunits <- function(from, to) {
    frU <- gsub("([-+0-9,. ])+([a-z]+)", "\\2", from)
    fr <- gsub("([-+0-9,. ]+)([a-z]+)", "\\1", from)
    fr <- as.numeric(gsub("([+-]?) *([^ ]*) *", "\\1\\2", fr))

    grepl(".*[0-9].*", to)
}

## inserts space 
expstr <- function(s, after, width, fill = " ", at) {
    ns <- nchar(s)
    space <- character(length(s))
    for (i in seq_along(space))
        space[i] <- paste(rep(" ", width[1L] - ns[i]), collapse = "")

    if (!missing(after))
        at <- as.numeric(regexpr(after, s)) + 1L
    paste(substr(s, 1L, at - 1L), space,
          substr(s, at, ns), sep = "")    
}
