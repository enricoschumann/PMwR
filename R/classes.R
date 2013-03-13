## * INSTRUMENT
Instrument <- function(type, id = NULL, ...) {

    type <- tolower(type)
    properties <- list(...)
    if (type == "equity") {

        I <- list(id = id,
                  description = properties$description,
                  modelValue = properties$modelValue,
                  marketValue = properties$marketValue,
                  currency = properties$currency)


        class(I) <- c("Equity", "Instrument")

    } else if (type == "vanillaoption") {
        class(I) <- c("VanillaOption", "Option", "Instrument")
    } else {
        stop("unknown Instrument type")
    }
    I
}



## * CASHFLOW
Cashflow <- function(cashflows, times) {
    if (length(cashflows) != length(times))
        stop("'cashflows' and 'times' need to have the same length")
    x <- list(cashflows = cf, times = times)
    class(x) <- "Cashflow"
    x
}
summary.Cashflow <- function(x) {
    res <- list(n = length(x$cashflows),
                tmin = min(x$times),
                tmax = max(x$times)
                )
    class(res) <- "summary.Cashflow"
    res
}
print.Cashflow <- function(x) {
    print(unclass(x))
    invisible(x)
}
print.summary.Cashflow <- function(x) {
    cat("\n",  x$n," cashflows; ",
        "first at time ", x$tmin, ", last at ", x$tmax, ".\n", sep = "")
    invisible(x)
}

                                        # example
tm <- 1:3
cf <- c(3,3,103)
c1 <- Cashflow(cf, tm)
summary(c1)




## TRADELIST
Tradelist <- function(datetime, notional, price, id, instrument, account) {
    
    if (missing(id))
        id <- NA
    if (missing(instrument))
        instrument <- NA
    if (missing(account))
        account <- NA
    
    ans <- list(id = id,
                instrument = instrument,
                account = account,
                datetime = datetime,
                notional = notional,
                price = price)
    class(ans) <- "Tradelist"
    ans    
}

print.Tradelist <- function(x, ...) {
    oo <- getOption("scipen")
    options(scipen = 1e8)
    on.exit(options(scipen = oo))

    dspT <- 10 ## display trades, instruments
    dspI <- 8
    
    if (all(!is.na(x$account)))
        rn <- x$account        
    if (all(!is.na(x$id)))
        rn <- paste(rn, x$id, sep = " | ")

    print(head(data.frame(datetime = x$datetime,
                          instrument = x$instrument,
                          notional = x$notional,
                          price = x$price,
                          row.names = seq_len(length(x$notional)),
                          stringsAsFactors = FALSE), dspT))
    if ((n <- length(x$notional)) > dspT)
        cat("...\n")
    insts <- sort(unique(x$instrument))
    if (length(insts) > dspI) {
        insts <- insts[seq_len(dspI)]
        insts[dspI] <- "..."
    }
    cat(paste0("(", n, " trades in ",
               paste(insts, collapse=", "), 
               ")\n"))
    invisible(x)
}
sort.Tradelist <- function(x, ...) {
    cat("not implemented\n")
    invisible(x)
}
filterTradelist <- function(x, datetime, notional, price,
                            id, instrument, account, ...) {    
    cat("not implemented\n")
    invisible(x)
}
c.Tradelist <- function(...) {
    tls <- list(...)
    if (length(tls) > 1L) {
        ans <- as.data.frame(unclass(tls[[1L]]), stringsAsFactors = FALSE)
        for (i in 2:length(tls))
            ans <- rbind(ans, as.data.frame(unclass(tls[[i]]),
                                            stringsAsFactors = FALSE))            
        Tradelist(id = ans$id,
                  instrument = ans$instrument,
                  account = ans$account,
                  datetime = ans$datetime,
                  notional = ans$notional,
                  price = ans$price)
    } else
        tls        
}
position <- function(x, when, from, to,...) {
    ## returns a vector
    x$instrument[is.na(x$instrument)] <- "not specified"
    pos <- numeric(length(nm <- sort(unique(x$instrument))))
    names(pos) <- nm
    for (i in seq_along(nm)) {
        ri  <-  nm[i] == x$instrument
        idt <- x$datetime[ri]
        iv  <- x$notional[ri]
        if (is.unsorted(idt)) {
            io  <- order(idt)
            idt <- idt[io]
            iv  <- id[io]
        }
        beforewhen <- which(when >= idt)
        if (length(beforewhen))
            pos[nm[i]] <- cumsum(iv)[max(beforewhen)]
    }
    pos
}

PLsorted <- function(x, ...) {
    UseMethod("PLsorted")
}

PLsorted.Tradelist <- function(x, allprices = NULL, alltimes = NULL,
                               initcash = 0, do.sort = FALSE) {
    allinstr <- unique(x$instrument)
    ans <- vector("list", length = length(allinstr))
    for (i in seq_along(allinstr)) {
        ii <- allinstr[i] == x$instrument
        position <- cumsum(x$notional[ii])
        pf <- x$price[ii] * position
        wealth <- pf + cumsum(-x$notional[ii] * x$price[ii]) + initcash        
        ans[[i]] <- list(position = position, wealth = wealth)
    }
    names(ans) <- allinstr
    ans
}
PLsorted.default <- function(notional, prices, tradetimes = NULL,
                             allprices = NULL, alltimes = NULL,
                             initcash = 0, do.sort = FALSE) {

    PMwR:::PLsorted(notional, prices, tradetimes = NULL,
                    allprices = NULL, alltimes = NULL,
                    initcash = 0, do.sort = FALSE)

}


if (FALSE) {




    require("Infront")
    tmp <- getInfrontTradesP("Jaro", from = "20130215000000", to = "20130215230000")
    tmp0 <- tmp[tmp$ticker == "GBL201303",]
    jb <- Tradelist(datetime = tmp$datetime, tmp$volume, tmp$price, id = tmp$rownames,
                    instrument = tmp$ticker, account = "JB")
    jb

    PLsorted(tmp0$volume, tmp0$price, tmp0$datetime)$wealth
    PMwR::PLsorted(tmp0$volume, tmp0$price, tmp0$datetime)$wealth


    
    ## example Tradelist
    x1 <- Tradelist(datetime = "20130114120000",
                   notional = c(9000, 60000, 6500, 9000, 4500, 5500, 9000),
                   price = c(61.23, 9.04, 82.52, 60.76, 130.5622, 101.50, 61.16),
                   instrument = c("bei", "dte", "fre", "hen3",  "lin",  "mrk", "sap"),
                   account = "Modulor")

    x2 <- Tradelist(datetime = "20130205120000",
                    notional = c(7000, 50000, 3500, 5500, 7500, 4500, 7000),
                    price = c(64.8606, 8.7785, 133.0005, 91.6590, 65.5398, 102.0192,60.4702),
                    instrument = c("bei", "dte", "lin", "fre",  "hen3",  "mrk", "sap"),
                    account = "Modulor")

    x3 <- Tradelist(datetime = "20130215120000",
                    notional = c(22000, -8000, 16000),
                    price = c(24.0063, 131.3805, 33.5474),
                    instrument = c("meo", "lin", "sdf"),
                    account = "Modulor")
    X <- c(x1,x2,x3)
    p1 <- position(X, "20130114120000")
    p2 <- position(X, "20130205120000")
    p3 <- position(X, "20130215120000")             

    from <- "20130114120000"; to <- "20130205120000"
    data <- getTablesSelect(c(names(p1), "dax"), "ib",
                            from = from,
                            to   = to,
                            columns = "close")
    t1 <- data$times
    p <- c(p1,0)
    portfolio1 <- data$close %*% p
    plot(portfolio1 <- portfolio1/portfolio1[1L], type = "l")

    
    from <- "20130205120000"; to <- "20130215120000"
    data <- getTablesSelect(c(names(p2), "dax"), "ib",
                            from = from,
                            to   = to,
                            columns = "close")
    t2 <- data$times
    p <- c(p2,0)
    portfolio2 <- data$close %*% p
    plot(portfolio2 <- portfolio2/portfolio2[1L], type = "l")

    from <- "20130215120000"; to <- format(Sys.time(), "%Y%m%s%H%M%S")
    data <- getTablesSelect(c(names(p3), "dax"), "ib",
                            from = from,
                            to   = to,
                            columns = "close")
    t3 <- data$times
    p <- c(p3,0)
    portfolio3 <- data$close %*% p
    plot(portfolio3 <- portfolio3/portfolio3[1L], type = "l")

    chain <- function(...) {
        L <- list(...)
        r <- na.locf(L[[1L]])
        if ((n <- length(L)) > 1L) {
            for (i in 2:n) {
                r  <- c(r[1:(length(r)-1)], r[length(r)] * na.locf(L[[i]]))
            }
        }
        r
    }
    PP <- chain(portfolio1, portfolio2, portfolio3)
    times <- char2time(unique(sort(c(t1,t2,t3))))


    from <- "20130114120000"; to <- format(Sys.time(), "%Y%m%s%H%M%S")
    data <- getTablesSelect("dax", "ib",
                            from = from,
                            to   = to,
                            columns = "close")


    png("~/Trading/aktien1.png", width = 600, height = 400)
    par(bty = "n",las = 1,mar=c(5,5,1,0), tck = 0.003)
    tmp <- plotTradingHours(x = 100*PP,
                            t = times,
                            interval = "5 min", labels = "days",
                            fromHHMMSS = "090000", col= "goldenrod3",
                            toHHMMSS   = "173000", ylim = c(97,110),
                            do.plotAxis = TRUE)
    legend(x = "topleft", c("Portfolio", "DAX"), bg = "white", lwd = 2,
       col = c("goldenrod3", grey(0.3)), bty="n")
    mp <- tmp$map(times)
    lines(mp$t, 
          100*data$close[mp$ix]/data$close[mp$ix][1L],
          col = grey(0.3))
    dev.off()



    
    ##myfund <- Fund("de000a0dpkd3", "MODULOR LSE 1", "EUR")


    ## methods
    ## tValue: berechnet theoretischen Wert (+ Griechen, Yields, etc)
    tValue <- function(x, ...)
        UseMethod("tValue")

    tValue.default <- function(x, ...)
        stop("don't know how to valuate ", deparse(substitute(x)))

    tValue.Instrument <- function(x, ...)
        list(tValue = x$tValue)

    tValue.Fund <- function(x, ...)
        list(tValue = x$tValue)

    tValue.Position <- function(P, ...) {}
    tValue.Portfolio <- function(P,  ...) {}

    mValue.default <- function(x, ..., aux) {
        NULL
    }
    aux <- identity
    mValue.Instrument <- function(x, ..., aux) {
        fargs <- aux(...)    
    }

    ## sort

    ## toLatex


}


sd(returns(get.hist.quote("^GDAXI", start = "2013-01-14", quote="Close")))*16
