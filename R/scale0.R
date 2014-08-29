## -*- truncate-lines: t; -*-
## Time-stamp: <2014-08-29 08:21:36 CEST (es)>

scale1 <- function (x, ...)
    UseMethod("scale1", x)

scale1.default <- function (x, ..., when = "first.complete",
                            level = 1, centre = FALSE, scale = FALSE) {

    makevec <- FALSE
    if (!is.matrix(x)) {
        x <- as.matrix(x)
        makevec <- TRUE
    }    
    if (when == "first.complete") {
        init.p <- min(which(apply(x, 1, function(i) !any(is.na(i)))))
        ## TODO: add check if all NA
        ## faster? rowSums(is.na(matrix(0, nrow=5,ncol=2)))
    } else if (when == "first") {
        init.p <- 1
    } else if (is.numeric(when)) {
        init.p <- when
    }
    if (centre) {
        x0 <- returns(x, pad = 0)
        m <- colMeans(x0[-1L, , drop = FALSE])
        for (i in seq_len(ncol(x0)))
            x[,i] <- cumprod(1+x0[ ,i] - m[i])
    }
    if (scale) {
        x0 <- returns(x, pad = 0)
        s <- apply(x0[-1L, , drop = FALSE], 2, sd)
        for (i in seq_len(ncol(x0)))
            x[,i] <- cumprod(1+x0[ ,i]/s[i] * scale)
    }
    for (i in seq_len(ncol(x)))
        x[,i] <- x[,i]/x[init.p, i]
    x <- level*x
    if (makevec)
        x <- c(x) 
    x

}

scale1.zoo <- function(x, ..., when = "first.complete",
                            level = 1, centre = FALSE, scale = FALSE) {

    ii <- index(x)
    x <- coredata(x)
    ans <- scale1.default(x, when = when, level = level,
                          centre = centre, scale = scale)
    zoo(ans, ii)
}


scale0 <- function(x, when = "first.complete", first = 100, scale = FALSE) {
    ZOO <- FALSE
    if (inherits(x, "zoo")) {
        ZOO <- TRUE
        ii <- index(x)
        x <- coredata(x)
    }
    makevec <- FALSE
    if (!is.matrix(x)) {
        x <- as.matrix(x)
        makevec <- TRUE
    }
    
    if (when == "first.complete") {
        init.p <- min(which(apply(x, 1, function(i) !any(is.na(i)))))
        ## TODO: add check if all NA        
    } else if (when == "first") {
        init.p <- 1
    } else if (is.numeric(when)) {
        init.p <- when
    }
    for (i in seq_len(ncol(x)))
        x[,i] <- x[,i]/x[init.p, i]
    if (scale) {
        x0 <- returns(x, pad = 0)
        s <- apply(x0, 2, sd)
        for (i in seq_len(ncol(x0)))
            x0[ ,i] <- x0[ ,i]/s[i] * scale
        for (i in seq_len(ncol(x)))
            x[,i] <- cumprod(1+x0[ ,i])
    }
    if (makevec)
        x <- c(first*x) else x <- first*x
    if (ZOO)
        x <- zoo(x, ii)
    x
}
