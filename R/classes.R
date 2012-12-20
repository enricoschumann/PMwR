## Instruments
createInstrument <- function(type, id = NULL, ...) {

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
}



## instrument: Fund

Instrument <- function(InstrId = "",
                       description = "",
                       currency = "",
                       tValue = NA) {
    result <- list(InstrId = InstrId,
                   description = description,
                   currency = currency,
                   tValue = tValue)
    class(result) <- "Instrument"
    result
}

Fund <- function(InstrId = "",
                 description = "",
                 currency = "",
                 tValue = NA) {
    result <- list(InstrId = InstrId,
                   description = description,
                   currency = currency,
                   tValue = tValue)
    class(result) <- c("Fund", "Instrument")
    result
}

myfund <- Fund("de000a0dpkd3", "MODULOR LSE 1", "EUR")


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



## Trade
Trade <- function(TradeId, portfolio, instr, datetime, notional, price) {


} 
## sort

## toLatex
