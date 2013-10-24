## -*- truncate-lines: t; -*-
## Time-stamp: <2013-10-23 13:57:04 CEST (es)>


rsplot <- function(x, y, ...,  xlab = NULL, ylab = NULL,
                   col.axis = grey(0.7), col.lines = grey(0.7),
                   fg = grey(0.5), bg = grey(0.8)) {
    dots <- list(...)
    xlab <- if (!missing(x) && is.null(xlab)) 
        deparse(substitute(x))
    ylab <- if (!missing(y) && is.null(ylab)) 
        deparse(substitute(y))
    
    par(tck = 0.005, las = 1)
    plot(x, y, cex = 0.01, asp = 1,
         xaxt = "n", yaxt = "n", bty = "n", pch = 1,
         xlab = xlab, ylab = ylab, ...)
    abline(v = qx <- quantile(x, c(0.01, 0.99)),
           h = qy <- quantile(y, c(0.01, 0.99)),
           col = col.lines)
    tmp <- c(0, qx, min(x), max(x))
    axis(1, at = tmp, labels = format(round(100*tmp,1), nsmall = 1),
         col = col.axis)
    tmp <- c(0, qy, min(y), max(y))
    axis(2, at = tmp, labels = format(round(100*tmp,1), nsmall = 1),
         col = col.axis)
    lines(x, y, cex = 0.8, type = "p",
          bg = bg, fg = fg, pch = 21)
}
    
x <- rnorm(100)*0.05
y <- rnorm(100)*0.02
rsplot(x,y, bg = "goldenrod3")
