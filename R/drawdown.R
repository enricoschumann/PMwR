## -*- truncate-lines: t; -*-
## Time-stamp: <2013-07-13 14:44:33 CEST (es)>

drawdown <- function(v, relative = TRUE, summary = TRUE) {
    cv  <- cummax(v)
    rd  <- cv - v
    if (relative)
        rd  <- rd/cv
    troughTime <- which.max(rd)
    peakTime <- which.max(v[seq_len(troughTime)])
    list(maximum = max(rd),
         high = v[peakTime],
         highPosition = peakTime,
         low = v[troughTime],
         lowPosition = troughTime)
}
