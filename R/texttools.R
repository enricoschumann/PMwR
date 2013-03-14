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


## TeX
## convert from one TeXunit to another
TeXunits <- function(from, to) {
    frU <- gsub("([-+0-9,. ])+([a-z]+)", "\\2", from)
    fr <- gsub("([-+0-9,. ]+)([a-z]+)", "\\1", from)
    fr <- as.numeric(gsub("([+-]?) *([^ ]*) *", "\\1\\2", fr))

    grepl(".*[0-9].*", to)
}
