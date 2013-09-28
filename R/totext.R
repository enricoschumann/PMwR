## -*- truncate-lines: t; -*-
## Time-stamp: <2013-09-28 13:03:02 CEST (es)>

toText <- function(x, ...)
    UseMethod("toText")

toText.default <- function(x, ...) 
    capture.output(write(x, ""))

