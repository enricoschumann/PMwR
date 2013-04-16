if (FALSE) {
    require("database")    

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



    require("Infront")
    tmp <- getInfrontTradesP("Jaro", from = "20130215000000", to = "20130215230000")
    tmp0 <- tmp[tmp$ticker == "GBL201303",]
    jb <- Tradelist(datetime = tmp$datetime, tmp$volume, tmp$price, id = tmp$rownames,
                    instrument = tmp$ticker, account = "JB")
    jb




    ## example Tradelist
    x1 <- Tradelist(timestamp = "20130114120000",
                    amount = c(9000, 60000, 6500, 9000, 4500, 5500, 9000),
                    price = c(61.23, 9.04, 82.52, 60.76, 130.5622, 101.50, 61.16),
                    instrument = c("bei", "dte", "fre", "hen3",  "lin",  "mrk", "sap"),
                    account = "Modulor")

    x2 <- Tradelist(timestamp = "20130205120000",
                    amount = c(7000, 50000, 3500, 5500, 7500, 4500, 7000),
                    price = c(64.8606, 8.7785, 133.0005, 91.6590, 65.5398, 102.0192,60.4702),
                    instrument = c("bei", "dte", "lin", "fre",  "hen3",  "mrk", "sap"),
                    account = "Modulor")

    x3 <- Tradelist(timestamp = "20130215120000",
                    amount = c(22000, -8000, 16000),
                    price = c(24.0063, 131.3805, 33.5474),
                    instrument = c("meo", "lin", "sdf"),
                    account = "Modulor")

    X <- c(x1,x2,x3)
    p1 <- position(X, when="20130114120000")
    p2 <- position(X, when="20130205120000")
    p3 <- position(X, when="20130215120000")             

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
                            t = index(alld),type="s",
                            interval = "5 min", labels = "days",
                            fromHHMMSS = "090000", col= "goldenrod3",
                            toHHMMSS   = "173000", ylim = c(97,112),
                            do.plotAxis = TRUE)
    legend(x = "topleft", c("Portfolio", "DAX"), bg = "white", lwd = 2,
           col = c("goldenrod3", grey(0.3)), bty="n")
    mp <- tmp$map(index(alld))
    lines(mp$t, 
          coredata(100*alld[mp$ix,1]/coredata(alld[mp$ix,1][1L])),
          col = grey(0.3))
    dev.off()

    ## long short
    x4 <- Tradelist(timestamp = "20130322120000",
                    amount =     c(-7*25, 11500, 19000, 6000, 50000, 4500, 5800, 3600),
                    price = c(0, 36.9341, 22.8104, 71.1379, 8.4771, 94.2888, 73.1623, 116.1301),
                    instrument = c("fdax201306", "sdf", "meo", "bei", "dte", "fre", "hen3", "mrk"),
                    account = "Modulor")

    
    to <- format(Sys.time(), "%Y%m%d%H%M%S")
    p1 <- position(x4, when=to)
    from <- "20130322120000"
    data <- getTablesSelect(names(p1), "ib",
                            from = from,
                            to   = to,
                            columns = "open")

    pr <- data$open[keep <- apply(data$open, 1, function(x) if (sum(is.na(x)) > 5) FALSE else TRUE), ]
    ti <- data$times[keep]

    pr[,names(p1)== "fdax201306"] <- pr[,names(p1)== "fdax201306"]-7925.2143
    portfolio1 <- pr %*% p1
    portfolio1 <- portfolio1/portfolio1[1L]

    png("~/Trading/aktienLS.png", width = 600, height = 400)
    par(bty = "n",las = 1,mar=c(5,5,1,0), tck = 0.003)
    tmp <- plotTradingHours(x = 100*portfolio1,
                            t = char2time(ti),type = "s",
                            holidays = as.Date(c("2013-03-29", "2013-04-01")),
                            interval = "5 min", labels = "days",
                            fromHHMMSS = "090000", col= "goldenrod3",
                            toHHMMSS   = "173000",
                            do.plotAxis = TRUE)
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




##                 in %
## RHOEN-KLINIKUM  15.0
## Suedzucker AG I 15.0
## Talanx AG Namen 15.0
## Kabel Deutschla 12.3
## TAG Immobilien  12.0
## SGL CARBON SE I  9.4
## Fielmann AG Inh  8.3
## Gagfah S.A. Act  6.5
## GSW Immobilien   5.9
## Symrise AG Inha  0.5

    png("~/Trading/aktienMDAX.png", width = 600, height = 400)

    ids <- c("de0007042301", "de0007297004", "de000tlx1005",
             "de000kd88880", "de0008303504", "de0007235301",
             "de0005772206", "lu0269583422", "de000gsw1111",
             "de000sym9999")
    w <- c(15.0, 15.0, 15.0, 12.3, 12.0, 9.4, 8.3, 6.5, 5.9, 0.5)/100
    w <- w/sum(w)
    data <- getTablesSelect(ids, "daily",
                            from = "20130311",
                            to   = "20130412",
                            columns = "close")

    u <- w/data$close[1,]
    plot(char2time(data$times), data$close %*% u, type = "l",
         ylim = c(0.96,1.04), xlab = "", ylab = "", col = "goldenrod3")
    sd(returns(data$close %*% u))*16
    
    data <- getTablesSelect("de0008467416", "indices",
                            from = "20130311",
                            to   = "20130412",
                            columns = "close")

    lines(char2time(data$times), data$close/data$close[1L], col = "blue")
    sd(returns(data$close/data$close[1L]))*16
    legend(x="topleft", legend = c("Portfolio", "MDAX"),
           col= c("goldenrod3","blue"), lwd=2)
    dev.off()
}
