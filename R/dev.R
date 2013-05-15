if (FALSE) {

    
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


    ## * TIMESERIES
    ## index (t) is sorted, but not unique
    require("fastmatch")
    require("zoo")
    sts <- function(t, x, check.sorted = TRUE) {
        if (length(t) != NROW(x))
            stop("lengths of t and x differ")
        z <- list(t = t, x = x)
        class(z) <- "sts"
        z
    }
    ##window.sts
    N1 <- 100000
    N2 <- 200000
    t1 <- sts(1:N1, rnorm(N1))
    t2 <- sts(sort(runif(N2))*N1, rnorm(N2))
    t1z <- zoo(t1$x, t1$t)
    t2z <- zoo(t2$x, t2$t)
    merge.sts <- function(...) {
        args <- list(...)
        ## TODO: check classes of indices
        T <- sort(unlist(lapply(args, `[[`, "t"), use.names = FALSE))
        X <- array(NA, dim = c(length(T), length(args)))
        for (j in seq_along(args)) {
            i <- fmatch(args[[j]][["t"]], T)
            X[cbind(i,j)] <- args[[j]][["x"]]
        }
        sts(T, X)
    }
    STS <- merge.sts(t1,t2)
    merge(t1z,t2z)


    

    require("Infront")
    tmp <- getInfrontTradesP("Jaro", from = "20130215000000", to = "20130215230000")
    tmp0 <- tmp[tmp$ticker == "GBL201303",]
    jb <- Tradelist(datetime = tmp$datetime, tmp$volume, tmp$price, id = tmp$rownames,
                    instrument = tmp$ticker, account = "JB")
    jb

    
    amount <- c(1,1,2,2)
    price <- c(101,102,103,104)
    all.equal(twReturns(price, amount), returns(price))

    amount <- c(1,1,1,2,2,0)
    dim(amount) <- c(3,2)
    price <- c(100,100,100,100,100,100)
    dim(price) <- c(3,2)
    twReturns(price, amount)
    all.equal(twReturns(price, amount), returns(price[,1]))

    amount <- c(1,1,1,2,2,2)
    dim(amount) <- c(3,2)
    price <- c(101,102,103,103,105,107)
    dim(price) <- c(3,2)
    all.equal(twReturns(price, amount), returns(rowSums(price*amount)))

    amount <- c(1,1,1,2,2,0)
    dim(amount) <- c(3,2)
    price <- c(101,102,103,103,105,107)
    dim(price) <- c(3,2)
    twReturns(price, amount)
    
    


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
                            to   = "20130508",
                            columns = "close")

    u <- w/data$close[1,]
    plot(char2time(data$times), data$close %*% u, type = "l",
         ylim = c(0.96,1.04), xlab = "", ylab = "", col = "goldenrod3")
    sd(returns(data$close %*% u))*16
    
    data <- getTablesSelect("de0008467416", "indices",
                            from = "20130311",
                            to   = "20130508",
                            columns = "close")

    lines(char2time(data$times), data$close/data$close[1L], col = "blue")
    sd(returns(data$close/data$close[1L]))*16
    legend(x="topleft", legend = c("Portfolio", "MDAX"),
           col= c("goldenrod3","blue"), lwd=2)
    dev.off()








          # BINARY SEARCH
    bs <- function(x, t, duplicates = "undef") {
        imin <- 1L 
        imax <- length(x)
        count <- 0L
        if (duplicates == "undef") {
            while (imax >= imin) {
                imid <- as.integer(imin + (imax - imin)/2)
                message("step ", count <- count + 1L, " -- ", imid)
                if (x[imid] > t)
                    imax <- imid - 1L
                else if (x[imid] < t)
                    imin <- imid + 1L
                else
                    break
            }
            imid
        } else if (duplicates == "last") {
            while (imax >= imin) {
                imid <- as.integer(imin + (imax - imin)/2)
                message("step ", count <- count + 1L, " -- ", imid)
                if (x[imid] > t)
                    imax <- imid - 1L
                else
                    imin <- imid + 1L
            }
            imid
        }
    }
    x <- sort(c(rnorm(1000000), 0.5))
    t <- 0.5    
    system.time(bs(x,t, "undef"))
    system.time(match(t, x))
    
    x <- rep(1, 10000)
    t <- 1
    bs(x,t)
    bs(x,t, "last")

    
    x <- 1:1000
    x[599:820] <- 700
    t <- 700
    bs(x,t, "undef")
    bs(x,t, "last")



    bs <- function(x, t) {
        lo <- 1L 
        hi <- length(x)
        count <- 0
        while (lo <= hi) {
            mid <- as.integer(lo + (hi - lo)/2)
            message("step ", count <- count + 1, "  ", mid)
            if (x[mid] > t)
                hi <- mid - 1L
            else if (x[mid] < t)
                lo <- mid + 1L
            else break
        }
        mid
    }



    x <- sort(c(rnorm(1000000), 0.5))
    t <- 0.5    
    bs(x,t)
    
    x <- rep(1, 10000)
    t <- 1
    bs(x,t)

    
    x <- 1:1000
    x[599:820] <- 700
    t <- 700
    bs(x,t)
    t <- 821.1
    x[bs(x,t)]



    
          # intraday

    


    
}
