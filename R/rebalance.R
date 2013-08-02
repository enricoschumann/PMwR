rebalance <- function(current, target, price,
                      notional = NULL, truncate = TRUE,
                      match.names = TRUE) {
    if (match.names == TRUE &&
        (is.null(names(price)) ||
         is.null(names(current)) ||
         is.null(names(target))))
        stop(sQuote("match.names"),
             " is TRUE but vectors are not named")
        
    if (is.null(notional))
        if (match.names)
            notional <- sum(current*price[names(current)])
        else
            notional <- sum(current*price)
    
    if (match.names)
        ans <- target * notional / price[names(target)]
    else 
        ans <- target * notional / price

    if (truncate)
        ans <- round(trunc(ans/10^(-truncate))*10^(-truncate))
    rbl <- list(current = current,
                target = ans, 
                names.current = names(current),
                names.target = names(target),
                price = price,
                notional = notional,
                match.names = match.names)
    class(rbl) <- "rebalance"
    rbl
}

print.rebalance <- function(x, ...) {
    all.names <- names(x$price)
    if (x$match.names) {
        target <- current <- numeric(length(all.names))        
        current[match(x$names.current, all.names)] <- x$current
        target[match(x$names.target, all.names)] <- x$target
        
    } else {
        if (is.null(all.names))
            all.names <- seq_along(x$price)
        current <- x$current
        target <- x$target               
    }
    print(data.frame(price  = x$price,
                     current = current,
                     `value` = current * x$price,
                     `% `  = format(100*current * x$price/x$notional, nsmall = 1, digits=1),
                     `  `  = format("     ", justify = "centre"),
                     new = target,
                     value = target * x$price,
                     `% `   = format(100*target * x$price / x$notional, nsmall = 1, digits =1),
                     `  ` = format("     ", justify = "centre"),
                     order = target - current,
                     row.names = all.names,
                     check.names = FALSE), ...)
    invisible(x)
}

price <- c(a = 1, b = 2, c = 3)
current <- c(a = 100, b = 20)
target <- c(a = 0.2, c = 0.3)
rebalance(current, target, price)

price <- c(1,2,3)
current <- c(100, 20, 0)
target <- c(0.2, 0, 0.3)
rebalance(current, target, price, match.names = FALSE)
