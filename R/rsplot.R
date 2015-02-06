## -*- truncate-lines: t; -*-
## Time-stamp: <2015-02-04 18:10:17 CET (es)>


rsplot <- function(x, y, ...,  xlab = NULL, ylab = NULL,
                   col.axis = grey(0.7), col.lines = grey(0.7),
                   fg = grey(0.5), bg = grey(0.8)) {
    dots <- list(...)
    xlab <- if (!missing(x) && is.null(xlab)) 
        deparse(substitute(x))
    ylab <- if (!missing(y) && is.null(ylab)) 
        deparse(substitute(y))
    
    par(tck = 0.005, las = 1)
    plot(x, y, asp = 1,
         xaxt = "n", yaxt = "n", bty = "n",
         xlab = xlab, ylab = ylab, ...)
    abline(v = qx <- quantile(x, c(0.01, 0.95)),
           h = qy <- quantile(y, c(0.01, 0.95)),
           col = col.lines)
    tmp <- c(0, qx, min(x), max(x))
    axis(1, at = tmp, labels = format(round(100*tmp,1), nsmall = 1),
         col = col.axis)
    tmp <- c(0, qy, min(y), max(y))
    axis(2, at = tmp, labels = format(round(100*tmp,1), nsmall = 1),
         col = col.axis)
    lines(x, y, type = "p",
          bg = bg, fg = fg, ...)
}
    
