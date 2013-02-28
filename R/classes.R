## Instruments
newInstrument <- function(type, id = NULL, ...) {

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





if (FALSE) {
    makeCashflow <- function(cf, times) {
        if (length(cf) != length(times))
            stop("'cf' and 'times' need to have the same length")
        obj <- list(cashflows = cf, times = times)
        class(obj) <- "Cashflow"
        obj
    }
    summary.Cashflow <- function(obj) {
        res <- list(n = length(obj$cashflows),
                    tmin = min(obj$times),
                    tmax = max(obj$times)
                    )
        class(res) <- "summary.Cashflow"
    }

    print.Summary.Cashflow <- function(obj) {
        print("number of cashflows :", obj$n, quote = FALSE)
        invisible(obj)
    }

    ## EXAMPLE

    tm <- 1:3
    cf <- c(3,3,103)
    c1 <- makeCashflow(cf, tm)
    summary(c1)




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
    position <- function(x, when, ...) {
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

    tmp <- getInfrontTradesP("Jaro", from = "20130215000000", to = "20130215230000")
    tmp0 <- tmp[tmp$ticker == "GBL201303",]
    jb <- Tradelist(datetime = tmp$datetime, tmp$volume, tmp$price, id = tmp$rownames,
                    instrument = tmp$ticker, account = "JB")
    jb

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

    PLsorted(tmp0$volume, tmp0$price, tmp0$datetime)$wealth
    PMwR::PLsorted(tmp0$volume, tmp0$price, tmp0$datetime)$wealth


    ## example Tradelist
    x <- Tradelist(datetime = "20130114120000",
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
    X <- c(x,x2,x3)
    PLsorted(X)
}
