makeHHMMSS <- function(x, label = "time specification (HHMMSS)") {
    x <- as.character(x)
    if (nchar(x) == 1L)
        x <- paste("0", x, "0000", sep = "")
    if (nchar(x) == 2L)
        x <- paste(x, "0000", sep = "")
    if (nchar(x) == 4L)
        x <- paste(x, "00", sep = "")

    ss <- substr(x, 1, 2)
    if (ss > "24" || ss < "00")
        stop("check ", label)
    ss <- substr(x, 3, 4)
    if (ss > "60" || ss < "00")
        stop("check ", label)
    ss <- substr(x, 5, 6)
    if (ss > "60" || ss < "00")
        stop("check ", label)
    x
}
wait <- function(x) 
    if (length(x) == 1L)
        Sys.sleep(x) else
            Sys.sleep(runif(1L, min(x), max(x)))
