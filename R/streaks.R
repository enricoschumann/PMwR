## -*- truncate-lines: t; -*-
## Copyright (C) 2018  Enrico Schumann

x <- sample(c(-0.01, 0.01), 20, replace = TRUE)

x <- c(0.01, -0.01, -0.01, 0.01, 0.01, 0.01, 0.01,
       0.01, 0.01, 0.01, 0.01, -0.01, -0.01, 0.01,
       -0.01, -0.01, -0.01, -0.01, -0.01, 0.01)


streaks <- function(x, ...)
    UseMethod("streaks")

streaks.default <- function(x, ...) {

}

streaks.zoo <- function(x, ...) {

}

streaks.NAVseries <- function(x, ...) {

}
