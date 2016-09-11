## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

amount <- function(x, unit) {
    ans <- x

    if ( is.null(dim(ans)) &&
         length(ans) > 1L &&
         length(unit) == 1L) {
        unit <- rep(unit, length(ans))
    }
    
    if (!is.null(dim(ans)) &&
        dim(ans)[[2]] > 1L &&
        length(unit) == 1L) {
        unit <- rep(unit, ncol(ans))
    }

    attr(ans, "unit") <- unit
    class(ans) <- "amount"
    ans
}

amount(1:10, unit = "EUR")

x <- 1:10
dim(x) <- c(10,1)
amount(x, unit = "EUR")

dim(x) <- c(5,2)
amount(x, unit = c("EUR", "USD"))

print.amount <- function (x, ...) {
    xx <- x
    attr(xx, "unit") <- NULL
    unit <- attr(x, "unit")
    if (!is.null(dim(xx)))
        colnames(xx) <- unit else names(xx) <- unit
    print(prettyNum(unclass(xx)))
    invisible(x)
}

rep.amount <- function(x, ...) {
    browser()
    unit <- attr(x, "unit")
    xx <- rep(unclass(x), ...)
    uu <- rep(unit, ...)
    amount(xx, uu)
}


convert <- function(amount, to, conv.table) {


}
    

## amount(c(100,200), c("EUR", "USD"))

conv_table <- function(table, unit, timestamp = NULL) {
    

}

## journal(amount = x)
