rebalance <- function(current, target, price,
                      notional = NULL, multiplier = 1, truncate = TRUE,
                      match.names = TRUE,
                      check.match = TRUE) {

    
    if (!match.names && length(current) == 1L && current == 0) {
        current <- rep.int(current, length(target))
        names(current) <- names(target)
    }

    if (!match.names && length(target) == 1L) {
        target <- rep.int(target, length(current))
        names(target) <- names(current)
    }

    
    if (!match.names &&
        (length(current) != length(target) ||
         length(current) != length(price) ||
         length(target)  != length(price))) {
        stop(sQuote("current"), ", ", sQuote("target"), " and ",
             sQuote("price"), " must have same length")
    }

    if (match.names) {
        if (is.null(names(price)) ||
            (is.null(names(current)) && !identical(unname(current), 0)) ||
            is.null(names(target))) {
            stop(sQuote("match.names"),
                 " is TRUE but vectors are not named")
        }
        if (check.match) {
            if (any(is.na(match(names(current), names(price)))))
                stop("name in current that does not match price")
            if (any(is.na(match(names(target), names(price)))))
                stop("name in target that does not match price")
        }
    }


    if (match.names &&
        is.null(names(multiplier)) &&
        length(multiplier) == 1L) {

        multiplier <- rep(multiplier, length(price))
        names(multiplier) <- names(price)
    }

    
    if (is.null(notional))
        if (match.names)
            notional <- sum(current * price[names(current)] *
                                multiplier[names(current)])
        else
            notional <- sum(current * price * multiplier)
    
    if (match.names)
        ans <- target * notional / price[names(target)] / multiplier[names(target)]
    else 
        ans <- target * notional / price / multiplier

    if (truncate)
        ans <- round(trunc(ans/10^(-truncate))*10^(-truncate))
    rbl <- list(current = current,
                target  = ans, 
                names.current = names(current),
                names.target  = names(target),
                price       = price,
                notional    = notional,
                match.names = match.names)
    class(rbl) <- "rebalance"
    rbl
}

print.rebalance <- function(x, ..., drop.zero = TRUE) {
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
    df <- data.frame(price  = x$price,
                     current = current,
                     `value` = current * x$price,
                     `% `   = format(100*current * x$price/x$notional, nsmall = 1, digits=1),
                     `  `   = format("     ", justify = "centre"),
                     new = target,
                     value = target * x$price,
                     `% `   = format(100*target * x$price / x$notional, nsmall = 1, digits =1),
                     `  ` = format("     ", justify = "centre"),
                     order = target - current,
                     row.names = all.names,
                     check.names = FALSE)

    if (drop.zero)
        df <- df[df$current != 0 | df$new != 0, ]
    print(df, ...)

    cat("\nNotional: ", x$notional, ".  Amount invested: ", sum(target * x$price),
        ".  Total turnover: ", sum(abs(current - target) * x$price),
        ".\n", sep = "")
    invisible(x)
}

## price <- c(a = 1, b = 2, c = 3)
## current <- c(a = 100, b = 20)
## target <- c(a = 0.2, c = 0.3)
## rebalance(current, target, price)

## price <- c(1,2,3)
## current <- c(100, 20, 0)
## target <- c(0.2, 0, 0.3)
## rebalance(current, target, price, match.names = FALSE)
