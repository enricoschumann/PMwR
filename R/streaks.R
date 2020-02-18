## -*- truncate-lines: t; -*-
## Copyright (C) 2018-20  Enrico Schumann

streaks <- function(x, ...)
    UseMethod("streaks")

streaks.default <- function(x,
                            up =  0.2, down = -up,
                            initial.state = NA,
                            y = NULL,
                            ...) {

    start <- 1
    end <- NA
    state <- tolower(initial.state)
    results <- data.frame(start = numeric(0),
                          end = numeric(0),
                          state = character(0))
    if (is.null(y)) y <- rep.int(1, length(x))
    if (is.na(state)) {
        hi <- x[1]/y[1]
        lo <- x[1]/y[1]
        hi.t <- 1
        lo.t <- 1
    } else if (state == "up") {
        hi <- x[1]/y[1]
        lo <- NA
        hi.t <- 1
        lo.t <- NA
    } else if (state == "down") {
        hi <- NA
        lo <- x[1]/y[1]
        hi.t <- NA
        lo.t <- 1
    }
    for (t in 2:length(x)) {
        dx <- x[t]/x[t - 1] / (y[t]/y[t - 1])
        xy.i <- x[t]/y[t]
        if (is.na(state)) {
            if (dx >= 1) {

                if (xy.i > hi) {
                    hi <- xy.i
                    hi.t <- t
                }
                if ( (x[t]/x[lo.t]) / (y[t]/y[lo.t]) - 1 >= up) {
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

            } else if (dx < 1) {

                if (xy.i < lo) {
                    lo <- xy.i
                    lo.t <- t
                }
                if ( (x[t]/x[hi.t]) / (y[t]/y[hi.t]) - 1 <= down) {
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
                        start <- hi.t
                    }
                }
            }

        } else if (state == "up") {

            if (dx >= 1) {
                if (xy.i > hi) {
                    hi <- xy.i
                    hi.t <- t
                }
            } else if (dx < 1 && (x[t]/x[hi.t]) / (y[t]/y[hi.t]) - 1 < down) {
                results <- rbind(results,
                                 data.frame(start = start,
                                            end = hi.t,
                                            state = state))
                state <- "down"
                start <- hi.t
                lo.t <- t
                lo <- xy.i
                hi.t <- NA
                hi <- NA
            }

        } else if (state == "down") {

            if (dx <= 1) {
                if (xy.i < lo) {
                    lo <- xy.i
                    lo.t <- t
                }
            } else if (dx > 1 && (x[t]/x[lo.t]) / (y[t]/y[lo.t]) - 1 > up) {
                results <- rbind(results,
                                 data.frame(start = start,
                                            end = lo.t,
                                            state = state))
                state <- "up"
                start <- lo.t
                lo.t <- NA
                lo <- NA
                hi.t <- t
                hi <- xy.i
            }

        }
    }
    results <- rbind(results,
                     data.frame(start = start,
                                end = length(x),
                                state = state))
    results[["return"]] <- x[results$end]/x[results$start] /
                          (y[results$end]/y[results$start]) - 1
    results
}

streaks.zoo <- function(x,
                        up   =  0.2,
                        down = -up,
                        initial.state = NA,
                        y = NULL, ...) {
    t <- index(x)
    ans <- streaks.default(x = coredata(x),
                           up   = up,
                           down = down,
                           initial.state = initial.state,
                           y = coredata(y), ...)
    ans$start <- t[ans$start]
    ans$end   <- t[ans$end]
    ans
}

streaks.NAVseries <- function(x,
                              up   =  0.2,
                              down = -up,
                              initial.state = NA,
                              bm = NULL, ...) {
    xx <- as.zoo(x)
    streaks.zoo(xx,
                up   = up, down = down,
                initial.state = initial.state, y = bm, ...)
}
