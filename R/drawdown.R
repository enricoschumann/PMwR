## -*- truncate-lines: t; -*-
## Time-stamp: <2013-07-29 15:41:24 CEST (es)>

drawdown <- function(v, relative = TRUE, summary = TRUE) {
    cv  <- cummax(v)
    rd  <- cv - v
    if (relative)
        rd  <- rd/cv
    troughTime <- which.max(rd)
    peakTime <- which.max(v[seq_len(troughTime)])
    list(maximum       = max(rd),
         high          = v[peakTime],
         high.position = peakTime,
         low           = v[troughTime],
         low.position  = troughTime)
}
