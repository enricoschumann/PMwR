value <- function(x, ...)
    UseMethod("value")

value.default <- function(x, ...) 
    stop("no method for ", class(x))

value.instrument <- function(x, ..., prepareArgs) {



}

prepareArgs <- function(x, ...)
    UseMethod("prepareArgs")

prepareArgs.default <- function(x, ...)
    list(...)

instrument <- function(class, id = NULL, ...) {

    properties <- list(...)
    class <- tolower(class)

    if (class == "equity") {
        
        ans <- list(id          = id,
                    description = properties$description,
                    currency    = properties$currency,
                    vfun        = function(x,...) NA)
                
        class(ans) <- c("equity", "instrument")
        
    } else if (class == "vanillaoptioneuropean") {
        
        ans <- list(id          = id,
                    description = properties$description,
                    underlier   = properties$underlier,
                    currency    = properties$currency,
                    strike      = properties$strike,
                    expiry      = properties$expiry,
                    payoff      = properties$payoff,
                    vfun        = NMOF::vanillaOptionEuropean)
        
        class(ans) <- c("vanillaOptionEuropean", "instrument")

    } else
        stop("unknown instrument class ", sQuote(class))
    
    ans
}
length.instrument <- function(x)
    1L

## ii <- instrument(class       = "vanillaOptionEuropean",
##                  id          = "odax201309c8000",
##                  description = "ODAX 8000 Call Sep 2013",
##                  underlier   = "DAX",
##                  currency    = "EUR",
##                  strike      = 8000,
##                  expiry      = "2013-09-20",
##                  payoff      = "call")

    
## print.vanillaOptionEuropean <- function(x, ...) {
##     cat(x$description, "\n")
##     cat("underlier:", format(substr(x$underlier,
##                                     1,
##                                     min(12, nchar(x$underlier))),
##                              width = 12))
##     cat("|", format(toupper(x$payoff), width = 4), " ")    
##     cat("expires", x$expiry, "\n")
## }

