## -*- truncate-lines: t; -*-
## Copyright (C) 2008-18  Enrico Schumann

makeHHMMSS <- function(x, label = "time specification (HHMMSS)") {
    x <- as.character(x)
    if (nchar(x) == 1L)
        x <- paste("0", x, "0000", sep = "")
    if (nchar(x) == 2L)
        x <- paste(x, "0000", sep = "")
    if (nchar(x) == 4L)
        x <- paste(x, "00", sep = "")

    ss <- substr(x, 1, 2)
    if (ss > "24" || ss < "00")
        stop("check ", label)
    ss <- substr(x, 3, 4)
    if (ss > "60" || ss < "00")
        stop("check ", label)
    ss <- substr(x, 5, 6)
    if (ss > "60" || ss < "00")
        stop("check ", label)
    x
}

last <- function(x, by, index = FALSE) {
    lby <- length(by)
    rby <- by[lby:1L]
    if (index)
        lby - match(unique(by), rby) + 1L
    else
        x[lby - match(unique(by), rby) + 1L]
}

first <- function(x, by, index = FALSE) {
    if (index)
        match(unique(by), by)
    else
        x[match(unique(by), by)]
}

letter2month <- function(s, instrument = "option"){
    s <- toupper(s)
    if (instrument == "option") {
        meaning <- c("C 1", "C 2", "C 3", "C 4",  "C 5",  "C 6",
                     "C 7", "C 8", "C 9", "C 10", "C 11", "C 12",
                     "P 1", "P 2", "P 3", "P 4",  "P 5" , "P 6",
                     "P 7", "P 8", "P 9", "P 10", "P 11", "P 12")
        meaning[match(s, LETTERS[1:24])]
    } else if (instrument == "future") {
        meaning <- c("January", "February", "March", "April",
                     "May", "June", "July", "August", "September",
                     "October", "November", "December")
        meaning[match(s, c("F", "G", "H", "J", "K", "M",
                           "N", "Q", "U", "V", "X", "Z"))]
    } else
        stop("unknown instrument")
}

.match_or_next <- matchOrNext <- function(x, y) {
    pos <- match(x, y)
    NApos <- which(is.na(pos))
    for (i in NApos)
        if (length(tmp <- which(x[i] <= y)))
            pos[i] <- min(tmp)
    pos
}

.match_or_previous <- matchOrPrevious <- function(x, y) {
    pos <- match(x, y)
    NApos <- which(is.na(pos))
    for (i in NApos)
        if (length(tmp <- which(x[i] >= y)))
            pos[i] <- max(tmp)
    pos
}

lag <- function(x, k, pad = NA) {
    if (!is.null(dim(x))) {
        stop("please apply columnwise")
    } else c(rep(pad, k),
             x[seq_len(length(x)-k)])    
}

vname <- function(v, names) {
    ans <- c(v)
    names(ans) <- names
    ans    
}

sparkline <- function(x,
                      width = 8,
                      height = 1.75,  ## renewcommand sparklineheight
                      lwd = 0.2,      ## setlength sparklinethickness
                      col = "black",  ## definecolor sparklinecolor, not supported
                      true.min = min(x),
                      true.max = max(x),
                      baseline = TRUE,
                      baseline.lwd = 0.1,     ## not supported
                      baseline.col = "black", ## not supported
                      points = 1000
                      ) {
    lx <- length(x)
    if (lx > points) {
        i <- seq(from = 1, to = length(x),
                 by = ceiling(lx/points))
        if (tail(i, 1) != length(x))
            i <- c(i, length(x))
        x <- x[i]
    }
    xx <- .map01(x, omin = true.min, omax = true.max)
    sl <- c("{",

            if (height != 1.75)
                paste0("\\renewcommand{\\sparklineheight}{", height, "}"),

            if (lwd != 0.2)
                paste0("\\setlength{\\sparklinethickness}{", lwd, " pt}"),

            paste0("\\begin{sparkline}{", width, "}"), ## ...

            "\\definecolor{sparklinecolor}{rgb}{0.7,0.7,0.7}",

            if (baseline) {
                c("\\spark ", paste(0, xx[1], 1, xx[1]), "/ \n")
            }, ## ...

            "\\definecolor{sparklinecolor}{named}{black}",

            "\\spark ",
            paste(seq(0,1, length.out = length(x)), xx),
            "/ \n",
            "\\end{sparkline}",
            "}" )    
    sl
}

sparkplot <- function(x, blocks = 7, xmin = NULL, xmax = NULL, ymin = NULL) {
    tmp <- hist(x, seq(min(x), max(x), length.out = blocks + 1L), plot = FALSE)
    if (.Platform$OS.type == "windows")
        warning("Unicode characters will probably not be correctly displayed.")
    
    bars <- c("\u2581","\u2582","\u2583","\u2584",
              "\u2585","\u2586","\u2587","\u2588")
    rx <- range(tmp$counts)
    if (!is.null(ymin))
        rx[1L] <- ymin
    d <- diff(rx)/(length(bars) - 1L)
    res <- paste(bars[(tmp$counts - rx[1L]) %/% d + 1L], collapse = "")
    cat(res, "\n")
    invisible(res)
}

.map01 <- function (x, min = 0, max = 1,
                   omin = min(x), omax = max(x)) {
    new.range <- max - min
    old.range <- omax - omin
    (new.range * x + min * omax - max * omin)/old.range
}

make_neighbour <- function(wmin = 0,
                           wmax = 1,
                           kmin = NULL,
                           kmax = NULL,
                           stepsize,
                           budget = TRUE,
                           random = TRUE,
                           update = FALSE,
                           R = NULL, ...) {

    if (!is.null(kmin))
        stop("no such neighbourhood yet")

    
    if (!isTRUE(budget) && length(budget) == 2L && !random && !update) {
        ## budget is a _range_ , i.e. a numeric vector of length two

        stop("not implemented")

    } else if (isTRUE(budget) && random && update) {

        stop("not implemented")

        ## if (length(wmin) > 1L || length(wmax) > 1L) {
        ##     if (length(wmin) == 1L)
        ##         wmin <- rep(wmin, length(wmax))
        ##     if (length(wmax) == 1L)
        ##         wmax <- rep(wmax, length(wmin))
        ##     function(w, ...) {
        ##         toSell <- which(w > wmin)
        ##         toBuy  <- which(w < wmax)
        ##         i <- toSell[sample.int(length(toSell), size = 1L)]
        ##         j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
        ##         stepsize <- runif(1) * stepsize
        ##         stepsize <- min(w[i] - wmin[i], wmax[j] - w[j], stepsize)
        ##         w[i] <- w[i] - stepsize
        ##         w[j] <- w[j] + stepsize
        ##         w
        ##     }
        ## } else {
        ##     function(w, ...) {
        ##         toSell <- which(w > wmin)
        ##         toBuy  <- which(w < wmax)
        ##         i <- toSell[sample.int(length(toSell), size = 1L)]
        ##         j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
        ##         stepsize <- runif(1) * stepsize
        ##         stepsize <- min(w[i] - wmin, wmax - w[j], stepsize)
        ##         w[i] <- w[i] - stepsize
        ##         w[j] <- w[j] + stepsize
        ##         w
        ##     }
        ## }

    } else if (isTRUE(budget) && random && !update) {
        if (length(wmin) > 1L || length(wmax) > 1L) {
            if (length(wmin) == 1L)
                wmin <- rep(wmin, length(wmax))
            if (length(wmax) == 1L)
                wmax <- rep(wmax, length(wmin))
            function(w, ...) {
                toSell <- which(w > wmin)
                toBuy  <- which(w < wmax)
                i <- toSell[sample.int(length(toSell), size = 1L)]
                j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
                stepsize <- runif(1) * stepsize
                stepsize <- min(w[i] - wmin[i], wmax[j] - w[j], stepsize)
                w[i] <- w[i] - stepsize
                w[j] <- w[j] + stepsize
                w
            }
        } else {
            function(w, ...) {
                toSell <- which(w > wmin)
                toBuy  <- which(w < wmax)
                i <- toSell[sample.int(length(toSell), size = 1L)]
                j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
                stepsize <- runif(1) * stepsize
                stepsize <- min(w[i] - wmin, wmax - w[j], stepsize)
                w[i] <- w[i] - stepsize
                w[j] <- w[j] + stepsize
                w
            }
        }
    } else if (budget && length(budget) == 1L && !random && !update) {
        if (length(wmin) > 1L || length(wmax) > 1L) {
            if (length(wmin) == 1L)
                wmin <- rep(wmin, length(wmax))
            if (length(wmax) == 1L)
                wmax <- rep(wmax, length(wmin))
            function(w, ...) {
                toSell <- which(w > wmin)
                toBuy  <- which(w < wmax)
                i <- toSell[sample.int(length(toSell), size = 1L)]
                j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
                stepsize <- min(w[i] - wmin[i], wmax[j] - w[j], stepsize)
                w[i] <- w[i] - stepsize
                w[j] <- w[j] + stepsize
                w
            }
        } else {
            function(w, ...) {
                toSell <- which(w > wmin)
                toBuy  <- which(w < wmax)
                i <- toSell[sample.int(length(toSell), size = 1L)]
                j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
                stepsize <- min(w[i] - wmin, wmax - w[j], stepsize)
                w[i] <- w[i] - stepsize
                w[j] <- w[j] + stepsize
                w
            }
        }
    } else if (budget && length(budget) == 2L && !random && !update) {
        if (length(wmin) > 1L || length(wmax) > 1L) {
            if (length(wmin) == 1L)
                wmin <- rep(wmin, length(wmax))
            if (length(wmax) == 1L)
                wmax <- rep(wmax, length(wmin))
            function(w, ...) {
                i <- sample.int(length(w), size = 1L)
                stepsize <- sample(c(-1,1), size = 1)*stepsize
                stepsize <- if (stepsize < 0) {
                    max(wmin[i]-w[i], stepsize, budget[1]-sum(w))
                } else {
                    min(wmax[i]-w[i], stepsize, budget[2]-sum(w))
                }
                w[i] <- w[i] + stepsize
                w
            }
        } else {
            function(w, ...) {
                i <- sample.int(length(w), size = 1L)
                stepsize <- sample(c(-1,1), size = 1)*stepsize
                stepsize <- if (stepsize < 0) {
                    max(wmin-w[i], stepsize, budget[1]-sum(w))
                } else {
                    min(wmax-w[i], stepsize, budget[2]-sum(w))
                }
                w[i] <- w[i] + stepsize
                w
            }
        }
    } else if (budget &&
               length(budget) == 1L &&
               !is.null(kmax) &&
               random
               && update) {

        if (is.null(R))
            stop(sQuote("R"), " must be provided when ",
                 sQuote("update"), " is TRUE")
        if (length(wmin) > 1L || length(wmax) > 1L) {
            if (length(wmin) == 1L)
                wmin <- rep(wmin, length(wmax))
            if (length(wmax) == 1L)
                wmax <- rep(wmax, length(wmin))
        }
        function(w, ...) {
            tol <- 1e-12
            x <- w[[1]]
            J <- sum(x > tol)
            if (J == kmax) 
                to_buy <- which(x > tol & w < wmax)
            else
                to_buy <- which(x < wmax)
            to_sell <- which(x > tol)
            i <- to_sell[sample.int(length(to_sell), size = 1L)]
            j <- to_buy[sample.int(length(to_buy), size = 1L)]
            eps <- runif(1) * stepsize
            eps <- min(x[i], wmax - x[j], eps)
            x[i] <- x[i] - eps
            x[j] <- x[j] + eps
            Rw <- x[[2]] + R[ , c(i, j)] %*% c(-eps, eps)
            list(w = x, Rw = Rw)
        }
        
    } else
        stop("no matches")
}

psim <- function(x, y) {

    stopifnot(length(x) == length(y))
    same.sign <- sign(x) == sign(y)

    list(same.assets = sum(same.sign),
         weight.overlap = sum(pmin(abs(x[same.sign]),
                                   abs(y[same.sign]))),
         max.abs.difference = max(abs(x-y)),
         mean.abs.difference = sum(abs(x-y))/length(x))
    
}

time.p_returns <- function(x, ...)
    attr(x, "t")

time.btest <- function(x, ...)
    x$timestamp

.timestamp <- function(x) {
    if (inherits(x, "p_returns"))
        attr(x, "t")
    else
        attr(x, "timestamp")
}

`.timestamp<-` <- function(x, value) {
    if (inherits(x, "p_returns"))
        attr(x, "t") <- value
    else
        attr(x, "timestamp") <- value
    x
}        

.may_be_Date <- function(x, ...) {
    ans <- try(as.Date(x), silent = TRUE)
    if (inherits(ans, "try-error"))
        FALSE
    else if (all(is.na(ans)))
        FALSE
    else
        TRUE
}

debug_prices <- function(nobs, na, base = 100) {    
    a <- base + seq_len(nobs)
    A <- array(a, dim = c(nobs, na))
    t(t(A) + seq(1, na) / 10^nchar(na))    
}


## base package has had a function 'isFALSE'
## since version R 3.5-0, and '.isFALSE'
## copies this functionality so that PMwR can
## be used with older versions of R
.isFALSE <- function(x)
    is.logical(x) && length(x) == 1L && !is.na(x) && !x
