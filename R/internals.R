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

letter2month <- function(s){
    s <- toupper(s)
    meaning <- c("C 1", "C 2", "C 3", "C 4",  "C 5",  "C 6",
                 "C 7", "C 8", "C 9", "C 10", "C 11", "C 12",
                 "P 1", "P 2", "P 3", "P 4",  "P 5" , "P 6",
                 "P 7", "P 8", "P 9", "P 10", "P 11", "P 12")
    meaning[match(s, LETTERS[1:24])]
}

insert <- function(x, list, values) {
    len <- length(list) * (length(values) - 1L) + length(x)
    ans <- vector(typeof(x), length = len)
    seq_len(len)
}

avg <- function(amount, price, tol = 1e-8) {
    if (any(rm <- abs(amount) < tol)) {
        warning("removed zero amounts")
        amount <-  amount[!rm]
        price  <- price[!rm]
    }
    cs <- cumsum(amount)
    acs <- abs(cs)
    av <- rd <- numeric(n <- length(cs))
    
    if (n == 1L) {
        list(average = price, realised = 0)            
    } else {
        av[1L] <- price[1L]
        for (i in 2L:n) {
            i1 <- i-1L
            if (acs[i] > acs[i1]) {
                av[i] <- (av[i1] * cs[i1] + price[i]*amount[i])/
                    (amount[i] + cs[i1])
                rd[i] <- rd[i1]
            } else {
                av[i] <- av[i1]
                rd[i] <- rd[i1] + amount[i] * (av[i] - price[i])
            }
        }
        list(average = av, realised = rd)            
    }
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

## scale0(x, when = "first.complete", start = 100,  center = TRUE, scale = TRUE)
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

lag <- function(x, k, pad = NA) {
    if (!is.null(dim(x))) {
        stop("please apply columnwise")
    } else c(rep(pad, k),
             x[seq_len(length(x)-k)])    
}

convertDate <- function(x, type) {
    type <- tolower(type)
    if (type == "excel"){
        as.Date(x, origin = "1899-12-30")              
    } else if (type == "matlab") {
        as.Date(x, origin = "1970-01-01") - 719529
    } else
        stop("unknown type")
}
