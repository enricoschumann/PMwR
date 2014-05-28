## -*- truncate-lines: t; -*-
## Time-stamp: <2014-05-28 19:59:03 CEST (es)>

## numbers <- sample(1:50,10, replace = TRUE)

## sparklines <- function(x, sep = "[ ,]+") {
##     if (length(x) == 1L)
##         x <- as.numeric(unlist(strsplit(x, sep)))
##     bars <- c("\u2581","\u2582","\u2583","\u2584",
##               "\u2585","\u2586","\u2587","\u2588")
##     d <- diff(r <- range(x))/(length(bars)-1)
##     cat(paste(bars[(x - r[1]) %/% d +1], collapse = ""), "\n")
## }
## "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1", "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"
## numbers <- "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1"
## sparklines(numbers)
## numbers <- "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"
## sparklines(numbers)


## x <- runif(1500)

sparkplot <- function(x, blocks = 7, xmin = NULL, xmax = NULL, ymin = NULL) {
    tmp <- hist(x, seq(min(x), max(x), length.out = blocks + 1L), plot = FALSE)
    if (.Platform$OS.type == "windows")
        warning("Unicode characters will probably not be correctly displayed.")
    
    bars <- c("\u2581","\u2582","\u2583","\u2584",
              "\u2585","\u2586","\u2587","\u2588")
    rx <- range(tmp$counts)
    if (!is.null(ymin))
        rx[1L] <- ymin
    d <- diff(rx)/(length(bars) - 1L)
    res <- paste(bars[(tmp$counts - rx[1L]) %/% d + 1L], collapse = "")
    cat(res, "\n")
    invisible(res)
}

x <- rnorm(1000)
sparkplot(x)
sparkplot(x, ymin = 0)

## sparkplot(rnorm(100))

bPlot <- function(x, width = 1, col = NULL) {
    ## TODO: allow width to be of any tex-unit
    ## TODO: 
    tmp <- pretty(x)
    lims <- c(min(tmp), max(tmp))
    xx <- map01(x, lims[1], lims[2]) * width
    ans <- paste0("\rule[0.1\baselineskip]{", xx, " cm}{0.3\baselineskip}")
    if (!is.null(col))
        ans <- paste0("\textcolor{", col, "}{", ans , "}")
    ans
}

map01 <- function(x, min, max) 
    (x - min)/(max - min)

## TeXunits(c("1 cm", "0.7 in"), c("in", "cm"))
## TeXunits(c("1 cm", "0.7 in"), "in")
## TeXunits("1 cm", c("in", "cm"))



## align
## character vector, pattern, at

align <- function(s, pattern, sep = " ", justify = "right", fixed = TRUE, at) {
    ans <- strsplit(s, pattern, fixed = fixed)
    nc <- lapply(ans, nchar)
    len <- max(unlist(lapply(nc, length)))
    res <- NULL
    if (length(justify) == 1)
        justify <- rep(justify, len)
    for (i in seq_len(len)) {
        width <- max(unlist(lapply(nc, `[`, i)), na.rm = TRUE)
        tmp <- unlist(lapply(ans, `[`, i))
        tmp[is.na(tmp)] <- ""
        res <- paste0(res, if (is.null(res)) "" else sep,
                      format(tmp,
                             width = width, justify = justify[i]))
    }
    res
}
s <- c("xxxxxxxxxxxxxxx",
       "1",
       "1.23|5.2|100000",
       "100|2|100")

cat(paste(align(s, "|", " | "), collapse = "\n"))
