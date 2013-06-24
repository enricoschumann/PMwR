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
