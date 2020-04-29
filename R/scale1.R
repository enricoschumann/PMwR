## -*- truncate-lines: t; -*-
## Copyright (C) 2008-19  Enrico Schumann

scale1 <- function (x, ...)
    UseMethod("scale1", x)

## TODO: make a wrapper scale100 that uses a default
##       level of 100?

scale1.default <- function (x, ...,
                            when = "first.complete",
                            level = 1,
                            centre = FALSE,
                            scale = FALSE,
                            geometric = TRUE,
                            total.g = NULL) {

    ## TODO: add a formal 'na.rm' argument

    makevec <- FALSE
    if (!is.matrix(x) && !is.data.frame(x)) {
        ## x is always coerced to matrix. But if it had no dim
        ## attribute, it will be coerced back to a vector at the
        ## end
        makevec <- TRUE
        x <- as.matrix(x)
    }
    if (when == "first.complete") {
        tmp <- which(apply(x, 1, function(i) !anyNA(i)))
        if (length(tmp))
            init.p <- min(tmp)
        else
            stop("no complete row (no row without NAs)")
        ## TODO faster? rowSums(is.na(matrix(0, nrow=5,ncol=2)))
    } else if (when == "first") {
        init.p <- 1
    } else if (when == "last") {
        init.p <- nrow(x)
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
            ## for (i in seq_len(ncol(x0))) {
            ##     tmp <- (1+x0[-1,i])/
            ##         (tail(x[ ,i], 1)/head(x[ ,i],1)) ^ (1/(length(x0[-1,i])))
            ##     x[,i] <- cumprod(c(1, tmp))
            ## }
            total.g <- 0
        } else {
            m <- colMeans(x0[-1L, , drop = FALSE], na.rm = TRUE)
            for (i in seq_len(ncol(x0)))
                x[,i] <- cumprod(1 + x0[ ,i] - m[i])
        }
        x[NAs] <- NA
    }
    if (!is.null(total.g)) {
        x0 <- .returns(x, lag = 1)
        if (length(total.g) == 1L)
            total.g <- rep(total.g, ncol(x))
        f <- function(z)
            sum(log(1+r+z)) - log(1+total.g[i])
        for (i in seq_len(ncol(x))) {
            r <- x0[,i]
            x[,i] <- cumprod(
                1 + c(0, r + uniroot(f, interval = c(-0.2,0.2), tol = 1e-10)$root))
        }
    }
    for (i in seq_len(ncol(x)))
        x[,i] <- x[,i]/x[init.p, i]
    x <- level*x
    if (makevec)
        x <- c(x)
    attr(x, "scale1_origin") <- init.p
    x

}

scale1.zoo <- function(x, ..., when = "first.complete",
                       level = 1, centre = FALSE, scale = FALSE,
                       geometric = TRUE,
                       inflate = NULL,
                       total.g = NULL) {

    ii <- index(x)
    x <- coredata(x)
    if (!is.null(inflate)) {
        dates <- as.Date(ii)
        inflate.d <- (1+inflate)^(1/365)
        x <- x*inflate.d^as.numeric(dates-dates[1L])
    }

    if (inherits(when, class(ii)) &&
       !(inherits(when, "character") && when == "first.complete"))
        when <- matchOrNext(when, ii)
    ans <- scale1.default(x, when = when, level = level,
                          centre = centre, scale = scale,
                          geometric = geometric,
                          total.g = total.g)
    orig <- attr(ans, "scale1_origin")
    ans <- zoo(ans, ii)
    attr(ans, "scale1_origin") <- ii[orig]
    ans
}

scale1.NAVseries <- function(x, ..., when = "first.complete",
                             level = 1, centre = FALSE, scale = FALSE,
                             geometric = TRUE,
                             inflate = NULL,
                             total.g = NULL) {

    ii <- attr(x, "timestamp")
    xx <- c(x)
    if (!is.null(inflate)) {
        dates <- as.Date(ii)
        inflate.d <- (1+inflate)^(1/365)
        xx <- xx*inflate.d^as.numeric(dates-dates[1L])
    }

    if (inherits(when, class(ii)))
        when <- matchOrNext(when, ii)
    ans <- scale1.default(xx, when = when, level = level,
                          centre = centre, scale = scale,
                          geometric = geometric,
                          total.g = total.g)
    x[] <- ans
    attr(x, "scale1_origin") <- ii[attr(ans, "scale1_origin")]
    x
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
        x[,i] <- x[, i]/x[init.p, i]
    if (scale) {
        x0 <- returns(x, pad = 0)
        s <- apply(x0, 2, sd)
        for (i in seq_len(ncol(x0)))
            x0[, i] <- x0[, i]/s[i] * scale
        for (i in seq_len(ncol(x)))
            x[, i] <- cumprod(1+x0[, i])
    }
    if (makevec)
        x <- c(first*x)
    else
        x <- first*x
    if (ZOO)
        x <- zoo(x, ii)
    x
}
