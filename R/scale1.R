## -*- truncate-lines: t; -*-

scale1 <- function (x, ...)
    UseMethod("scale1", x)

scale100 <- function (x, ..., level = 100)
    UseMethod("scale1", x)

scale1.default <- function (x, ..., when = "first.complete",
                            level = 1, centre = FALSE, scale = FALSE, geometric = TRUE) {

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
    if (scale) {
        NAs <- is.na(x)
        x0 <- returns(x, pad = NA)
        s <- apply(x0[-1L, , drop = FALSE], 2, sd, na.rm = TRUE)
        x0[is.na(x0)] <- 0
        for (i in seq_len(ncol(x0)))
            x[,i] <- cumprod(1+x0[ ,i]/s[i] * scale)
        x[NAs] <- NA
    }
    if (centre) {
        NAs <- is.na(x)
        x0 <- returns(x, pad = 0)

        ## m <- if (geometric) {
        ##     apply(x0, 2, function(x) (tail(z, 1)/head(z,1))^(1/(nrow(x)-1)))
        ##     ## exp(colMeans(log(1+x0[-1L, , drop = FALSE]), na.rm = TRUE)) - 1 
        ##     ((tail(z, 1)/head(z,1))^(1/length(zr)))
        ## } else
        ##     colMeans(x0[-1L, , drop = FALSE], na.rm = TRUE)
        ## x0[is.na(x0)] <- 0
        ## for (i in seq_len(ncol(x0)))
        ##     x[,i] <- cumprod(1 + x0[ ,i] - m[i])
        ## x[NAs] <- NA
        ## browser()
        if (geometric) {
            for (i in seq_len(ncol(x0))) {
                tmp <- (1+x0[-1,i])/
                    (tail(x[ ,i], 1)/head(x[ ,i],1)) ^ (1/(length(x0[-1,i])))
                x[,i] <- cumprod(c(1, tmp))
            }
        } else {
            m <- colMeans(x0[-1L, , drop = FALSE], na.rm = TRUE)
            for (i in seq_len(ncol(x0)))
                x[,i] <- cumprod(1 + x0[ ,i] - m[i])
        }
        x[NAs] <- NA
    }
    for (i in seq_len(ncol(x)))
        x[,i] <- x[,i]/x[init.p, i]
    x <- level*x
    if (makevec)
        x <- c(x) 
    x

}

scale1.zoo <- function(x, ..., when = "first.complete",
                       level = 1, centre = FALSE, scale = FALSE,
                       geometric = TRUE) {

    ii <- index(x)
    x <- coredata(x)
    if (inherits(when, class(ii)))
        when <- matchOrNext(when, ii)
    ans <- scale1.default(x, when = when, level = level,
                          centre = centre, scale = scale,
                          geometric = geometric)
    zoo(ans, ii)
}

scale0 <- function(x, when = "first.complete", first = 100, scale = FALSE) {
    .Deprecated("scale1", package = "PMwR")
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
