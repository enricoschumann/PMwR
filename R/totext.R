## -*- truncate-lines: t; -*-
## Time-stamp: <2013-10-16 09:05:58 CEST (es)>

toText <- function(x, ...)
    UseMethod("toText")

toText.default <- function(x, ...) 
    capture.output(write(as.character(x), ""))

