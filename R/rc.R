## -*- truncate-lines: t; -*-
## Copyright (C) 2021  Enrico Schumann

rc <- function(R, weights, timestamp, segments = NULL) {
    if (missing(weights))
        weights <- 1
    if (is.null(segments)) {
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
    ns <- length(segments)
    nt <- length(timestamp)
    df <- data.frame(timestamp,
                     cbind(weights*R, rowSums(weights*R)),
                     stringsAsFactors = FALSE)
    names(df) <- c("timestamp", segments, "total")

    later_r <- c(rev(cumprod(1 + rev(df[["total"]])))[-1], 1)

    total <- rep(NA_real_, ns + 1)
    names(total) <- c(segments, "total")
    for (i in seq_len(ns))
        total[[i]] <- sum(df[[i + 1]] * later_r)
    total[[ns + 1]] <- cumprod(df[["total"]] + 1)[[nt]] - 1
    list(period_contributions = df,
         total_contributions = total)
}
