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

wait <- function(x) 
    if (length(x) == 1L)
        Sys.sleep(x) else Sys.sleep(runif(1L, min(x), max(x)))

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

insert <- function(x, list, values) {
    len <- length(list) * (length(values) - 1L) + length(x)
    ans <- vector(typeof(x), length = len)
    seq_len(len)
}

matchOrNext <- function(x, y) {
    pos <- match(x, y)
    NApos <- which(is.na(pos))
    for (i in NApos)
        if (length(tmp <- which(x[i] <= y)))
            pos[i] <- min(tmp)
    pos
}

matchOrPrevious <- function(x, y) {
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

sparklines <- function(x,
                       width = 8,
                       sparklineheight = 1.75,
                       true.min = min(x),
                       true.max = max(x),
                       zero.line = TRUE) {
    xx <- map01(x, omin = true.min, omax = true.max)
    sl <- c("{",
            if (sparklineheight != 1.75)
                paste0("\\renewcommand{\\sparklineheight}{", sparklineheight, "}"),
            paste0("\\begin{sparkline}{", width, "}"), ## ...
            
            ## zero.line
            if (zero.line) {
                c("\\spark ",
                  paste(0, xx[1], 1, xx[1]),
                  "/ \n")
            }, ## ...
            
            "\\spark ",
            paste(seq(0,1, length.out = length(x)), xx),
            "/ \n",
            "\\end{sparkline}",
            "}" )    
    sl
}

map01 <- function (x, min = 0, max = 1,
                   omin = min(x), omax = max(x)) {
    new.range <- max - min
    old.range <- omax - omin
    (new.range * x + min * omax - max * omin)/old.range
}
