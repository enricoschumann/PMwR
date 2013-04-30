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
last <- function(x, by, index = FALSE) {
    lby <- length(by)
    rby <- by[lby:1L]
    if (index)
        lby - match(unique(by), rby) + 1L
    else
        x[lby - match(unique(by), rby) + 1L]
}
first <- function(x, by, index = FALSE) {
    if (index)
        match(unique(by), by)
    else
        x[match(unique(by), by)]
}
rebalance <- function(current, target, prices, notional,
                      names = NULL,
                      w.current = FALSE, w.target = TRUE,
                      do.trunc = TRUE) {

    if (is.null(names))
        names <- names(current)
    if (missing(notional))
        notional <- sum(current*prices)
    ans <- target * notional / prices
    if (do.trunc)
        ans <- round(trunc(ans/10^(-do.trunc))*10^(-do.trunc))
    rbl <- list(current = current, new = ans, 
                names = names, prices = prices, notional = notional)
    class(rbl) <- "rebalance"
    rbl
}
print.rebalance <- function(x, ...) {
    print(data.frame(price      = x$prices,
                     current     = x$current,
                     `value` = x$current * x$prices,
                     `% `  = format(100*x$current * x$prices/x$notional, nsmall = 1),
                     `  ` = format("     ", justify = "centre"),
                     new = x$new,
                     value = x$new * x$prices,
                     `% `   = format(100*x$new * x$prices / x$notional, nsmall = 1),
                     `  ` = format("     ", justify = "centre"),
                     order = x$new - x$current,
                     row.names = x$names,
                     check.names = FALSE), ...)
    invisible(x)
}
