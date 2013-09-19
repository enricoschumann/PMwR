## -*- truncate-lines: t; -*-
## Time-stamp: <2013-09-17 13:36:40 CEST (es)>

rsplot <- function(x, y, ...,  xlab = "Index", ylab = "Portfolio") {
    require("MASS")
    dots <- list(...)
    par(tck = 0.005, las = 1)
    eqscplot(x, y, cex = 0.1,
             xaxt = "n", yaxt = "n", bty = "n", pch = 1,
             xlab = xlab, ylab = ylab, ...)
    abline(v = qx <- quantile(x, c(0.01,0.99)),
           h = qy <- quantile(y, c(0.01,0.99)),
           col = grey(.7))
    tmp <- c(0, qx, min(x), max(x))
    axis(1, at = tmp, labels = format(round(100*tmp,1), nsmall = 1),
         col = grey(0.7))
    tmp <- c(0, qy, min(y), max(y))
    axis(2, at = tmp, labels = format(round(100*tmp,1), nsmall = 1),
         col = grey(0.7))
    lines(x, y, cex = 0.8, type = "p",
          bg = grey(0.8), fg = grey(0.7), pch = 21)
}
    
