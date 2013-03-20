if (FALSE) {
    
                                        # rebalance

    ##
    ## rebalance: given a portfolio and weights, determine orders
    ##    

    s <-       c("DTE","SAP","SDF","MEO","MRKK","FRE","HEN3" ,"BEI"  ,"LIN"  ,"FME")
    current <- c(500  , 0   ,90   ,0    ,20    ,34   ,16     ,49     ,0      ,0)
    target  <- c(0.15,0.0,0.1,0,0.15,0.15,0.15,0.15,0,0.15)
    prices <- c(8.36,63.44 ,35.9325 ,21.2325 ,113.85 ,95.21 ,65.965 ,69.48 ,132.175 ,51.55)
    rebalance <- function(current, target, prices, notional,
                          w.target = TRUE, names = NULL) {

        if (missing(notional))
            notional <- sum(current*prices)
        ans <- trunc(target * notional / prices)
        data.frame(prices = prices, current = current,
                   value.now = current * prices, " " = "  =>  ", 
                   target = ans, value.then = ans * prices,
                   " " = "  =>  ", order = ans - current,
                   row.names = names,
                   check.names = FALSE)
    }
    rebalance(current, target, prices, notional = 50000, names= s)


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

    from <- "20130215120000"; to <- format(Sys.time(), "%Y%m%d%H%M%S")
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
    tmp <- zoo(PP, times)

    from <- "20130114120000"; to <- format(Sys.time(), "%Y%m%d%H%M%S")
    data <- getTablesSelect("dax", "ib",
                            from = from,
                            to   = to,
                            columns = "close")
    alld <- merge(zoo(data$close, char2time(data$times)), tmp)
    alld <- na.locf(alld)
    
    png("~/Trading/aktien1.png", width = 600, height = 400)
    par(bty = "n",las = 1,mar=c(5,5,1,0), tck = 0.003)
    tmp <- plotTradingHours(x = 100*alld[,2],
                            t = index(alld),
                            interval = "5 min", labels = "days",
                            fromHHMMSS = "090000", col= "goldenrod3",
                            toHHMMSS   = "173000", ylim = c(97,110),
                            do.plotAxis = TRUE)
    legend(x = "topleft", c("Portfolio", "DAX"), bg = "white", lwd = 2,
           col = c("goldenrod3", grey(0.3)), bty="n")
    mp <- tmp$map(index(alld))
    lines(mp$t, 
          coredata(100*alld[mp$ix,1]/coredata(alld[mp$ix,1][1L])),
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
