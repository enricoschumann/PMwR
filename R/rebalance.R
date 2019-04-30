## -*- truncate-lines: t; -*-
## Copyright (C) 2008-19  Enrico Schumann

rebalance <- function(current,
                      target,
                      price,
                      notional = NULL,
                      multiplier = 1,
                      truncate = TRUE,
                      match.names = TRUE,
                      fraction = 1,
                      drop.zero = FALSE,
                      current.weights = FALSE,
                      target.weights = TRUE) {

    if (inherits(current, "position")) {
        instr <- attr(current, "instrument")
        current <- as.numeric(current)
        names(current) <- instr
        current.weights <- FALSE
    }
    if (inherits(target, "position")) {
        instr <- attr(target, "instrument")
        target <- as.numeric(target)
        names(target) <- instr
        target.weights <- FALSE
    }

    if (length(current) == 1L &&
        current == 0 &&
        is.null(notional)) {

        stop(sQuote("notional"), " must be specified")

    }
    if (!match.names &&
        length(current) == 1L &&
        current == 0 &&
        length(target) == 1L) {

        ## current == 0 and target is a single number
        current <- rep.int(current, length(price))
        target <- rep.int(target, length(price))
        names(current) <- names(target) <- names(price)
    }

    if (length(current) == 1L &&
        current == 0 &&
        is.null(names(current))) {

        current <- rep.int(current, length(target))
        names(current) <- names(target)
    }

    if (length(target) == 1L &&
        is.null(names(target))) {
        target <- rep.int(target, length(current))
        names(target) <- names(current)
    }

    if (is.null(names(multiplier)) &&
        length(multiplier) == 1L) {
        multiplier <- rep(multiplier, length(price))
        names(multiplier) <- names(price)
    }

    if (match.names) {

        ## special case: current and target are of
        ## length 1 and unnamed
        if (length(current) == 1L &&
            current == 0 &&
            is.null(names(current)) &&
            length(target) == 1L &&
            is.null(names(target))) {
            current <- rep(0, length(price))
            target <- rep(target, length(price))
            names(current) <- names(target) <- names(price)
        }

        if (is.null(names(price)) ||
            (is.null(names(current)) && !identical(unname(current), 0)) ||
            is.null(names(target))) {
            stop(sQuote("match.names"),
                 " is TRUE but vectors are not named")
        }

        if (any(miss.name <- is.na(match(names(current),
                                         names(price))))) {
            warning("instrument in current without price: ",
                    if (sum(miss.name) > 3) "\n",
                    paste(names(current)[miss.name],
                          collapse = if (sum(miss.name) > 3) "\n" else ","),
                    immediate. = TRUE)
        }
        if (any(miss.name <- is.na(match(names(target),
                                         names(price))))) {
            warning("instrument in target without price: ",
                    if (sum(miss.name) > 3) "\n",
                    paste(names(current)[miss.name],
                          collapse = if (sum(miss.name) > 3) "\n" else ","),
                    immediate. = TRUE)
        }
        all.names <- sort(unique(
            c(names(target), names(current))))
        multiplier <- multiplier[all.names]
        target_ <- current_ <- numeric(length(all.names))
        current_[match(names(current), all.names)] <- current
        target_[match(names(target), all.names)] <- target
        current <- current_
        target <- target_
        price <- price[all.names]

    } else {
        if ( length(current) != length(target) ||
             length(current) != length(price)  ||
             length(target)  != length(price) ) {
                stop(sQuote("current"), ", ",
                     sQuote("target"), " and ",
                     sQuote("price"), " must have same length")
        }
        all.names <- NA
    }

    if (is.null(notional)) {
        if (current.weights && target.weights)
            stop(sQuote("notional"), " must be provided")
        if (target.weights)
            notional <- sum(current * price *
                            multiplier)
        else if (current.weights)
            notional <- sum(target * price *
                            multiplier)
    }

    if (current.weights)
        current <- notional*current/
            price/multiplier

    if (target.weights)
        target <- notional*target/
            price/multiplier
    if (truncate) {
        target <- round(trunc(target/10^(-truncate))*10^(-truncate))
        diff <- fraction*(target - current)
        diff <- round(trunc(diff/10^(-truncate))*10^(-truncate))
    } else
        diff <- fraction*(target - current)
    rbl <- data.frame(instrument = all.names,
                      price = price,
                      current = current,
                      target = target,
                      difference = diff,
                      stringsAsFactors = FALSE)
    attr(rbl, "notional") <- notional
    attr(rbl, "match.names") <- match.names
    attr(rbl, "multiplier") <-  multiplier

    if (drop.zero)
        rbl <- rbl[rbl$current != rbl$target, ]
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
                     `%`     = format(100*x$current * x$price / attr(x, "notional"),
                                      nsmall = 1, digits = 1),
                     `  `    = format("     ", justify = "centre"),
                     target  = x$target,
                     value   = x$target * x$price,
                     `%`     = format(100*x$target * x$price / attr(x, "notional"),
                                      nsmall = 1, digits = 1),
                     `  `    = format("     ", justify = "centre"),
                     order   = x$difference,
                     row.names = all.names,
                     check.names = FALSE)

    if (drop.zero)
        df <- df[df$current != df$target, ]
    print(df, ...)

    cat("\nNotional: ", attr(x, "notional"),
        ".  Target net amount : ", sum(x$target * x$price),
        ".  Turnover (2-way): ", sum(abs(x$current - x$target) * x$price),
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
