Instrument <- function(class, id = NULL, ...) {

    properties <- list(...)
    if (class == "Equity") {
        
        ans <- list(id = id,
                    description = properties$description,
                    modelValue = properties$modelValue,
                    marketValue = properties$marketValue,
                    currency = properties$currency)
        
        
        class(ans) <- c("Equity", "Instrument")
        
    } else if (class == "VanillaOptionEuropean") {
        
        ans <- list(id          = id,
                    description = properties$description,
                    underlier   = properties$underlier,
                    tvalue      = NULL,
                    mvalue      = NULL,
                    currency    = properties$currency,
                    strike      = properties$strike,
                    expiry      = properties$expiry,
                    payoff      = properties$payoff)
        
        class(ans) <- c("VanillaOptionEuropean", "VanillaOption",
                        "Option", "Instrument")
    } else
        stop("unknown Instrument class ", sQuote(class))
    
    ans
}
ii <- Instrument(class = "VanillaOptionEuropean", id = NA,
                 description = "ODAX 8000 Call Jun 2013",
                 underlier   = "DAX",
                 currency    = "EUR",
                 strike      = 8000,
                 expiry      = "2013-06-21",
                 payoff      = "call")

length.Instrument <- function(x)
    1L
    
print.VanillaOptionEuropean <- function(x, ...) {
    cat(x$description, "\n")
    cat("underlier:", format(substr(x$underlier,
                                    1,
                                    min(12, nchar(x$underlier))),
                             width = 12))
    cat("|", format(toupper(x$payoff), width = 4), " ")    
    cat("expires", x$expiry, "\n")
}

## value: berechnet theoretischen Wert (+ Griechen, Yields, etc)
value <- function(x, ...)
    UseMethod("value")

dots2args <- function(x, ...)
    UseMethod("dots2args")

dots2args.default <- function(x, ...)
    list(...)

## value.default <- function(x, ...)
##     stop("don't know how to valuate ", deparse(substitute(x)))


value.default <- function(x, ..., dots2args = list())
    do.call(x, dots2args(x, ...))
    
value.Instrument <- function(x, ..., dots2args)
    stop("implement me :-)")

##myfund <- Fund("de000a0dpkd3", "MODULOR LSE 1", "EUR")

