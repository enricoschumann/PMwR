## -*- truncate-lines: t; -*-

rebalance <- function(current,
                      target,
                      price,
                      notional = NULL,
                      multiplier = 1,
                      truncate = TRUE,
                      match.names = TRUE,
                      fraction = 1,
                      drop.zero = FALSE,
                      target.weights = TRUE) {


    if (inherits(current, "position")) {
        instr <- attr(current, "instrument")
        current <- as.numeric(current)
        names(current) <- instr
    }
    if (inherits(target, "position")) {
        instr <- attr(target, "instrument")
        target <- as.numeric(target)
        names(target) <- instr
        target.weights  <-  FALSE
    }
    
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

        ## TODO say for which instruments there is no price
        if (any(is.na(match(names(current), names(price)))))
            warning("instrument in current without price")
        if (any(is.na(match(names(target), names(price)))))
            warning("instrument in target without price")    
    }


    if (match.names &&
        is.null(names(multiplier)) &&
        length(multiplier) == 1L) {
        multiplier <- rep(multiplier, length(price))
        names(multiplier) <- names(price)
    }

    
    if (is.null(notional)) {
        if (match.names)
            notional <- sum(current * price[names(current)] *
                                multiplier[names(current)])
        else
            notional <- sum(current * price * multiplier)
    }
    
    if (match.names) {
        all.names <- sort(unique(c(names(target), names(current))))
        target_ <- current_ <- numeric(length(all.names))
        current_[match(names(current), all.names)] <- current
        target_[match(names(target), all.names)] <- target
        current <- current_
        price <- price[all.names]
        if (target.weights)
            ans <- target_ * notional / price / multiplier[all.names]
        else
            ans <- target_
    } else {
        if (target.weights)            
            ans <- target * notional / price / multiplier
        else
            ans <- target
        all.names <- NA
    }
    if (truncate) {
        ans <- round(trunc(ans/10^(-truncate))*10^(-truncate))
        diff <- fraction*(ans - current)
        diff <- round(trunc(diff/10^(-truncate))*10^(-truncate))
    } else
        diff <- fraction*(ans - current)
    rbl <- data.frame(instrument = all.names,
                      price = price,
                      current = current,
                      target = ans,
                      difference = diff,
                      stringsAsFactors = FALSE)
    attr(rbl, "notional") <- notional
    attr(rbl, "match.names") <- match.names
    attr(rbl, "multiplier") <-  multiplier

    if (drop.zero)
        rbl <- rbl[rbl$current != 0 | rbl$target != 0, ]
    class(rbl) <- c("rebalance", "data.frame")
    rbl
}

print.rebalance <- function(x, ..., drop.zero = TRUE) {

    sp <- getOption("scipen")
    on.exit(options(scipen = sp))
    options(scipen = 1e8)
    
    all.names <- x[["instrument"]]
    if (all(is.na(all.names)))
        all.names <- seq_along(x$current)
    df <- data.frame(price   = x$price,
                     current = x$current,
                     value   = x$current * x$price, ## TODO multiplier
                     `%`    = format(100*x$current * x$price / attr(x, "notional"),
                                     nsmall = 1, digits = 1),
                     `  `    = format("     ", justify = "centre"),
                     target  = x$target,
                     value   = x$target * x$price,
                     `%`    = format(100*x$target * x$price / attr(x, "notional"),
                                      nsmall = 1, digits = 1),
                     `  `    = format("     ", justify = "centre"),
                     order   = x$difference,
                     row.names = all.names,
                     check.names = FALSE)

    if (drop.zero)
        df <- df[df$current != 0 | df$target != 0, ]
    print(df, ...)

    cat("\nNotional: ", attr(x, "notional"),
        ".  Amount invested: ", sum(x$target * x$price),
        ".  Total (2-way) turnover: ", sum(abs(x$current - x$target) * x$price),
        ".\n", sep = "")
    invisible(x)
}

replace_weight <- function(weights, ..., prefix = TRUE, sep = "::") {
    repl <- list(...)
    for (i in seq_along(repl)) {
        nw <- names(weights)
        ii <- match(names(repl)[[i]], nw, nomatch = 0)
        if (ii > 0) {
            w_new <- weights[[ii]]*repl[[i]]
            if (prefix)
                names(w_new) <- paste0(names(weights)[[ii]], sep,
                                       names(repl[[i]]))
            tmp <- c(weights[0:(ii-1L)],
                         w_new)
            if (ii != length(weights))
                weights <- c(tmp, weights[ (ii+1L) : length(weights)])
            else
                weights <- tmp
        }
    }
    weights
}
