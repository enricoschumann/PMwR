## -*- truncate-lines: t; -*-
## Copyright (C) 2023  Enrico Schumann

rc <- function(R, weights, timestamp, segments = NULL,
               R.bm = NULL, weights.bm = NULL,
               method = "contribution",
               linking.method = NULL,
               allocation.minus.bm = TRUE,
               tol = sqrt(.Machine$double.eps)) {

    if (missing(weights))
        weights <- 1
    else if (is.null(dim(weights)))
        weights <- t(weights)

    if (is.null(dim(R)))
        R <- t(R)

    if (is.null(segments)) {

        ## TODO: segments could also be a matrix
        ##       (e.g. changing sectors over time),
        ##       or list of vectors (more than one
        ##       grouping)

        segments <- if (!is.null(cr <- colnames(R)))
                        cr
                    else if (!is.null(cr <- colnames(weights)))
                        cr
                    else
                        paste0("segment_", 1:ncol(weights))
    } else if (length(segments) != ncol(R))
        warning("length(segments) != ncol(R)")

    if (missing(timestamp))
        timestamp <- 1:nrow(R)
    else if (anyDuplicated(timestamp))
        stop("duplicated timestamps")
    else if (is.unsorted(timestamp)) {
        ## TODO make checks
        o <- order(timestamp)
        timestamp <- timestamp(o)
        R <- R[o, ]
        weights <- weights[o]
    }

    nt <- length(timestamp)

    if (any(duplicated(segments))) {
        A <- array(NA_real_, dim = c(nt, length(unique(segments))))
        colnames(A) <- sort(unique(segments))
        R <- tapply(R*weights, segments, sum)
        weights <- tapply(weights, segments, sum)
        R <- R/weights
        R <- R[segments]
        weights <- weights[segments]
    }

    ns <- length(segments)
    R0 <- R
    if (is.finite(tol))
        R0[is.finite(weights) & abs(weights) < tol] <- 0
    df <- data.frame(timestamp,
                     cbind(weights*R0, rowSums(weights*R0)),
                     stringsAsFactors = FALSE)
    names(df) <- c("timestamp", segments, "total")

    if (method == "contribution") {

        if (is.null(linking.method))
            linking.method <- "geometric0"

        if (linking.method == "geometric0") {

            later_r <- c(rev(cumprod(1 + rev(df[["total"]])))[-1], 1)

            total <- rep(NA_real_, ns + 1)
            names(total) <- c(segments, "total")
            for (i in seq_len(ns))
                total[[i]] <- sum(df[[i + 1]] * later_r)
            total[[ns + 1]] <- cumprod(df[["total"]] + 1)[[nt]] - 1

        } else if (linking.method == "geometric1") {

            earlier_r <- c(1,
                           cumprod(1 + df[["total"]][-nrow(df)]))

            total <- rep(NA_real_, ns + 1)
            names(total) <- c(segments, "total")
            for (i in seq_len(ns))
                total[[i]] <- sum(df[[i + 1]] * earlier_r)
            total[[ns + 1]] <- cumprod(df[["total"]] + 1)[[nt]] - 1

        } else if (linking.method == "logarithmic") {
            kt <- log(df[["total"]] + 1) / df[["total"]]
            k <- prod(df[["total"]] + 1)
            k <- log(k) / (k-1)
            adj_ct <- df[, -c(1, ncol(df))]*kt
            total <- colSums(adj_ct)/k
            total <- c(total, total = sum(total))
        }
        ans <- list(period_contributions = df,
                    total_contributions = total)
        attr(ans, "method") <- "contribution"

    } else if (method %in%
               c("attribution", "topdown", "bottomup")) {

        if (!is.null(linking.method))
            .NotYetUsed("linking.method", FALSE)

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
        if (is.null(dim(B)))
            B <- t(B)

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
                 stop("unknown method")

        ## SELECTION
        S <- if (method == "attribution" || method == "bottomup") {
                 weights.bm * (R - B)
             } else if (method == "topdown") {
                 weights * (R - B)
             } else
                 stop("unknown method")

        ## INTERACTION
        I <- if (method == "attribution") {
                 dw * (R - B)
             } else if (method %in% c("topdown", "bottomup")) {
                 array(0, dim = dim(R))
             } else
                 stop("unknown method")

        ## if (method == "bhb") {
        ##     alloc <-                     dw * R.bm   ## allocation
        ##     selec <-            weights.bm  * dR  ## selection
        ##     inter <-                     dw * dR  ## interaction

        ## } else if (method == "bf") {
        ##     alloc <-                     dw * (R.bm - R.bm.total)  ## allocation
        ##     selec <-            weights.bm  * dR  ## selection
        ##     inter <-                     dw * dR  ## interaction
        ## }

        ## if (is.character(interaction) && interaction == "top-down") {
        ##     ## asset allocation first: interaction is combined with selection
        ##     selec <-                weights * dR  ## selection
        ##     inter <- rep(0, length(weights))

        ## } else if (is.character(interaction) && interaction == "bottom-up") {
        ##     ## selection first: interaction is combined with allocation
        ##     if (method == "bhb")
        ##         alloc <- (weights - weights.bm) * R
        ##     else if (method == "bf")
        ##         alloc <- (weights - weights.bm) * (R - R.bm.total)

        ##     selec <-            weights.bm * dR  ## selection
        ##     inter <- rep(0, length(weights))
        ## }

        ans <- list(allocation  = c(A, rowSums(A)),
                    selection   = c(S, rowSums(S)),
                    interaction = c(I, rowSums(I)))
        names(ans$allocation)  <- c(segments, "total")
        names(ans$selection)   <- c(segments, "total")
        names(ans$interaction) <- c(segments, "total")
        attr(ans, "method") <- "attribution (default)"
        attr(ans, "linking.method") <- "none"


    } else
        stop("unknown method")

    ans
}
