## -*- truncate-lines: t; -*-
## Time-stamp: <2017-01-24 15:55:42 CET (es)>

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


