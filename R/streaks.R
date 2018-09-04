## -*- truncate-lines: t; -*-
## Copyright (C) 2018  Enrico Schumann

streaks <- function(x, ...)
    UseMethod("streaks")

streaks.default <- function(x, up =  0.2, down = -0.2,
                            initial.state = NA, ...) {

    start <- 1
    end <- NA
    state <- tolower(initial.state)
    results <- data.frame(start = numeric(0),
                          end = numeric(0),
                          state = character(0))

    if (is.na(state)) {
        hi <- x[1]
        lo <- x[1]
        hi.t <- 1
        lo.t <- 1
    } else if (state == "up") {
        hi <- x[1]
        lo <- NA
        hi.t <- 1
        lo.t <- NA
    } else if (state == "down") {
        hi <- NA
        lo <- x[1]
        hi.t <- NA
        lo.t <- 1
    }
    for (t in 2:length(x)) {
        dx <- x[t] - x[t - 1]
        x.i <- x[t]
        if (is.na(state)) {
            if (dx >= 0) {

                if (x.i > hi) {
                    hi <- x.i
                    hi.t <- t
                }
                if (x.i/lo - 1 >= up) {
                    state <- "up"
                    if (lo.t == 1) {
                        lo <- NA
                        lo.t <- NA
                        start <- 1
                    } else {
                        results <- rbind(results,
                                         data.frame(start = 1,
                                                    end = lo.t,
                                                    state = NA))
                        start <- lo.t
                    }
                }

            } else if (dx < 0) {

                if (x.i < lo) {
                    lo <- x.i
                    lo.t <- t
                }
                if (x.i/hi - 1 <= down) {
                    state <- "down"
                    if (hi.t == 1) {
                        hi <- NA
                        hi.t <- NA
                        start <- 1
                    } else {
                        results <- rbind(results,
                                         data.frame(start = 1,
                                                    end = hi.t,
                                                    state = NA))
                        start <- hi.t }}
            }

        } else if (state == "up") {

            if (dx >= 0) {
                if (x.i > hi) {
                    hi <- x.i
                    hi.t <- t
                }
            } else if (dx < 0 && x.i/hi -1 < down) {
                results <- rbind(results,
                                 data.frame(start = start,
                                            end = hi.t,
                                            state = state))
                state <- "down"
                start <- hi.t
                lo.t <- t
                lo <- x.i
                hi.t <- NA
                hi <- NA
            }

        } else if (state == "down") {

            if (dx <= 0) {
                if (x.i < lo) {
                    lo <- x.i
                    lo.t <- t
                }
            } else if (dx > 0 && x.i/lo -1 > up) {
                results <- rbind(results,
                                 data.frame(start = start,
                                            end = lo.t,
                                            state = state))
                state <- "up"
                start <- lo.t
                lo.t <- NA
                lo <- NA
                hi.t <- t
                hi <- x.i
            }

        }
    }
    results <- rbind(results,
                     data.frame(start = start, end = length(x), state = state))
    results[["return"]] <- x[results$end]/x[results$start] - 1
    results
}

streaks.zoo <- function(x,
                        up   =  0.2,
                        down = -0.2,
                        initial.state = NA, ...) {
    t <- index(x)
    x <- coredata(x)
    ans <- streaks.default(x,
                           up   = up,
                           down = down,
                           initial.state = initial.state, ...)
    ans$start <- t[ans$start]
    ans$end   <- t[ans$end]
    ans
}

streaks.NAVseries <- function(x,
                              up   =  0.2,
                              down = -0.2,
                              initial.state = NA, ...) {
    xx <- as.zoo(x)
    streaks.zoo(xx,
                up   = up, down = down,
                initial.state = initial.state, ...)
}
