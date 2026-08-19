## -*- truncate-lines: t; -*-
## Copyright (C) 2023-26  Enrico Schumann

rc <- function(R, weights, timestamp, segments = NULL,
               R.bm = NULL, weights.bm = NULL,
               method = "contribution",
               linking.method = NULL,
               allocation.minus.bm = TRUE,
               tol = sqrt(.Machine$double.eps),
               options = list()) {

    if (missing(method) &&
        (!is.null(R.bm) || !is.null(weights.bm))) {
        message("method switched to attribution")
        method <- "attribution"
    }

    if (is.null(dim(R)))
        R <- t(R)

    if (missing(weights))
        weights <- array(1, dim = dim(R))
    else if (is.null(dim(weights)))
        weights <- t(weights)

    if (!is.null(R.bm) && is.null(dim(R.bm)))
        R.bm <- t(R.bm)
    if (!is.null(weights.bm) && is.null(dim(weights.bm)))
        weights.bm <- t(weights.bm)

    if (!is.null(weights.bm) && is.null(R.bm))
        R.bm <- R


    if (is.null(segments)) {

        ## TODO segments could also be a matrix
        ## (e.g. changing sectors over time), or list of
        ## vectors (more than one grouping). [But if a
        ## list, it could be aggregated.]

        segments <-
            if (!is.null(cr <- colnames(R)))
                cr
            else if (!is.null(cr <- colnames(weights)))
                cr
            else
                paste0("segment_", 1:ncol(weights))
    } else if (length(segments) != NCOL(R))
        warning("length(segments) != ncol(R)")

    if (any(duplicated(segments))) {
        u.s <- sort(unique(segments))
        R <- t(tapply(R*weights, segments, sum))
        weights <- t(tapply(weights, segments, sum))
        segments <- u.s
    }

    if (missing(timestamp))
        timestamp <- seq_len(nrow(R))
    else if (anyDuplicated(timestamp))
        stop("duplicated timestamps")
    else if (is.unsorted(timestamp)) {
        o <- order(timestamp)
        R <- R[o, ]
        weights <- weights[o, ]
        timestamp <- timestamp[o]
    }

    nt <- length(timestamp)
    ns <- length(segments)


    if (method == "contribution") {

        R0 <- R
        if (is.finite(tol))
            R0[is.finite(weights) & abs(weights) < tol] <- 0
        df <- data.frame(timestamp,
                         cbind(weights*R0, rowSums(weights*R0)),
                         stringsAsFactors = FALSE)
        names(df) <- c("timestamp", segments, "total")

        if (is.null(linking.method))
            linking.method <- "1-cumulative"

        if (linking.method == "1-cumulative")
            linking.method <- "geometric1"
        else if (linking.method == "0-cumulative")
            linking.method <- "geometric0"

        if (linking.method == "geometric1") {

            later_r <-
                c(rev(cumprod(1 + rev(df[["total"]])))[-1], 1)

            total <- rep(NA_real_, ns + 1)
            names(total) <- c(segments, "total")
            ns1 <- seq_len(ns)
            total[ns1] <- colSums(as.matrix(df[, ns1 + 1, drop = FALSE]) * later_r)
            total[[ns + 1]] <- cumprod(df[["total"]] + 1)[[nt]] - 1

        } else if (linking.method == "geometric0") {

            earlier_r <-
                c(1, cumprod(1 + df[["total"]][-nrow(df)]))

            total <- rep(NA_real_, ns + 1)
            names(total) <- c(segments, "total")
            ns1 <- seq_len(ns)
            total[ns1] <- colSums(df[, ns1 + 1] * earlier_r)
            total[[ns + 1]] <- cumprod(df[["total"]] + 1)[[nt]] - 1

        } else if (grepl("geometric", linking.method)) {
            f <- 0.5
            later_r <-
                c(rev(cumprod(1 + rev(f*df[["total"]])))[-1], 1)
            earlier_r <-
                c(1,  cumprod(1 + (1-f)*df[["total"]][-nrow(df)]))

            total <- rep(NA_real_, ns + 1)
            names(total) <- c(segments, "total")
            ns1 <- seq_len(ns)
            total[ns1] <- colSums(df[, ns1 + 1] * earlier_r * later_r)
            total[[ns + 1]] <- cumprod(df[["total"]] + 1)[[nt]] - 1

        } else if (linking.method == "logarithmic") {
            C <- df[, -c(1, ncol(df))]
            total <- .linking_logarithmic(C,
                                          r = df[["total"]],
                                          b = 0)
            adj_ct <- attr(total, "C.adj")
            total <- c(total, total = sum(total))
        }
        ans <- list(period_contributions = df,
                    total_contributions = total)
        attr(ans, "method") <- "contribution"

    } else if (method %in%
               c("attribution", "topdown", "bottomup")) {

        if (any(duplicated(segments))) {
            R <- tapply(R*weights, segments, sum)
            weights <- tapply(weights, segments, sum)
            R <- R/weights
            R <- R[segments]
            weights <- weights[segments]
        }

        if (is.null(dim(weights.bm)))
            weights.bm <- t(weights.bm)

        B <- R.bm

        if (!is.null(segments))
            colnames(weights) <- colnames(weights.bm) <-
                colnames(R) <- colnames(B) <- segments

        B.total <- rowSums(weights.bm * B)
        R.total <- rowSums(weights * R)
        dw <- weights - weights.bm
        dR <- R - B


        ## ALLOCATION
        A <- if (method == "attribution" || method == "topdown") {
                 if (allocation.minus.bm)
                     dw * (B - B.total)
                 else
                     dw *  B
             } else if (method == "bottomup") {
                 if (allocation.minus.bm)
                     dw * (R - B.total)
                 else
                     dw *  R
             } else
                 stop("unknown method: ", method)

        ## SELECTION
        S <- if (method == "attribution" || method == "bottomup") {
                 weights.bm * (R - B)
             } else if (method == "topdown") {
                 weights * (R - B)
             } else
                 stop("unknown method: ", method)

        ## INTERACTION
        I <- if (method == "attribution") {
                 dw * (R - B)
             } else if (method %in% c("topdown", "bottomup")) {
                 array(0, dim = dim(R))
             } else
                 stop("unknown method: ", method)

        tA <- rowSums(A)
        tS <- rowSums(S)
        tI <- rowSums(I)

        total <- c("allocation"  = sum(tA),
                   "selection"   = sum(tS),
                   "interaction" = sum(tI),
                   "total" = sum(tA, tS, tI))
        ans <- list(allocation  = cbind(A, total = tA),
                    selection   = cbind(S, total = tS),
                    interaction = cbind(I, total = tI),
                    total       = total)

        colnames(ans$allocation)  <- c(segments, "total")
        colnames(ans$selection)   <- c(segments, "total")
        colnames(ans$interaction) <- c(segments, "total")
        attr(ans, "method") <-
            c(attribution = "attribution",
              topdown     = "attribution (top-down)",
              bottomup    = "attribution (bottom-up)")[[method]]

        if (!is.null(linking.method)) {

            tmp.C <- cbind(ans$allocation [, seq(1, ns)],
                           ans$selection  [, seq(1, ns)],
                           ans$interaction[, seq(1, ns)])
            tmp.C <- .carino1999(tmp.C, R.total, B.total)

            attr(ans, "linking.method") <- "Carino1999"
            attr(ans, "adjusted") <- list(
                "allocation"  = attr(tmp.C, "adjusted")[,      seq(1, ns)],
                "selection"   = attr(tmp.C, "adjusted")[, 1*ns+seq(1, ns)],
                "interaction" = attr(tmp.C, "adjusted")[, 2*ns+seq(1, ns)])

            ans$total <-
                rowSums(do.call(
                    rbind, lapply(attr(ans, "adjusted"), colSums)))
            ans$total <- c(ans$total, total = sum(ans$total))

        } else {
            attr(ans, "linking.method") <- "none"
        }


    } else
        stop("unknown method; should be 'contribution' or 'attribution'")

    class(ans) <- "rc"
    ans
}




## @Article{valtonen2002,
##   author       = {Erik Valtonen},
##   title        = {Incremental Attribution with and without
##                   Notional Portfolios},
##   journaltitle = {Journal of Performance Measurement},
##   year         = 2002,
##   volume       = 7,
##   number       = 1,
##   pages        = {68--83}
## }
.linking_cumulative0 <- function(C, r, b = 0, ...) {

    ## C .. matrix of contributions (or 'attributes')
    ## r .. period returns of portfolio
    ## b .. period returns of benchmark
    tr <- cumprod(1 + r)
    f <- c(1, tr[-length(tr)])  ## earlier
    C * f
}

.linking_cumulative1 <- function(C, r, b = 0, ...) {

    if (nrow(C) == 1L)
        return(C)

    i <- seq.int(from = length(r), to = 1, by = -1)
    f <- c(cumprod(1 + r[i])[i][-1L], 1)
    C * f
}

.linking_cumulativex <- function(C, r, b = 0, x = 0, ...) {

    ## C .. matrix of contributions (or 'attributes')
    ## r .. period returns of portfolio
    ## b .. period returns of benchmark

    R <- cumprod(1 + r) - 1
    B <- cumprod(1 + b) - 1

    rb <- r - b
    RB <- R - B

    n <- nrow(C)
    A <- array(NA, dim = dim(C))

    A[1, ] <- C[1, ]

    if (n > 1L) {
        if (!identical(b, 0) && length(b) == 1)
            b <- rep.int(b, n)

        if (identical(b, 0))
            for (i in 2:n) {
                A[i, ] <- A[i - 1, ] * (1 +         x  * r[i]) +
                          C[i,     ] * (1 +    (1 - x) * R[i-1])
            }
        else
            for (i in 2:n) {
                A[i, ] <- A[i - 1, ] * (1 + b[i  ] +      x  * rb[i]) +
                          C[i,     ] * (1 + B[i-1] + (1 - x) * RB[i-1])
            }
    }

    A
}



## D. R. Cari{\~n}o -- Combining Attribution Effects Over
## Time, 1999
.carino1999 <- .linking_logarithmic <- function(C, r, b = 0, ...) {

    ## C .. matrix of contributions (or 'attributes')
    ## r .. period returns of portfolio
    ## b .. period returns of benchmark

    rT <- prod(r + 1) - 1
    bT <- prod(b + 1) - 1

    r_b <- r - b
    kt <- log(1 + r) - log(1 + b)
    i <- abs(r_b) < 1e-14
    kt <- kt / r_b
    kt[i] <- 1/(1 + r[i])

    k  <- log(1 + rT) - log(1 + bT)

    rT_bT <- rT - bT
    if (abs(rT_bT) < 1e-14)
        k <- 1/(1 + rT) else k <- k / (rT - bT)

    C.adj <- C * kt / k
    total <- colSums(C.adj)
    attr(total, "adjusted") <- C.adj
    total
}


## @Article{colin2007,
##   author       = {Andrew Colin},
##   title        = {A Brinson Model Alternative: an Equity Attribution
##                   Model with Orthogonal Risk Contributions},
##   journal      = {Journal of Performance Measurement},
##   year         = 2007,
##   issue         = {fall}
## }
.colin2007 <- function(weights, weights.bm, R) {

    if (is.null(names(weights)) &&
        is.null(names(weights.bm)) &&
        is.null(names(R)))
        names(weights) <- names(weights.bm)  <- names(R) <- 1:length(R)


    n.w <- names(weights)
    n.bm <- names(weights.bm)
    n <- sort(unique(c(n.w, n.bm)))
    w_ <- w <- W <- numeric(length(n))
    names(w_) <- names(w) <- names(W) <- n
    r <- R[n]
    w[n.w] <- weights
    W[n.bm] <- weights.bm
    w_[w != 0 & W != 0] <- W[w != 0 & W != 0]
    w_ <- w_/sum(w_)

    df <- data.frame(portfolio = w*r,
                     benchmark = W*r,
                     selection = (w_ - W)*r,
                     allocation = (w - w_)*r)
    df <- rbind(df, apply(df, 2, sum))
    row.names(df)[nrow(df)] <- "total"
    df
}

## weights <- c(a=0.5,b=0.1,cash=0.4)
## weights.bm <- c(a=0.5,b=0.5)
## R <- c(a=0.02,b=-0.01, cash = 0)
## .colin2007(weights, weights.bm, R)

## weights <- c(0.8,0,0.2,0,0)
## weights.bm <- c(.3,.3,.1,.1,.2)
## R <- c(2,-2,1,-2,0)/100
## dput(.colin2007(weights, weights.bm, R))
